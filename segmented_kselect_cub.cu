#include <cstdio>
#include <vector>
#include <random>
#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <cub/cub.cuh>
#include "utils.cuh"

// -----------------------------------------------------
// Små værktøjer (host)
// -----------------------------------------------------

static void fill_random_u32(std::vector<uint32_t>& v, uint64_t seed = 1234567ULL) {
  std::mt19937_64 rng(seed ^ 0x9e3779b97f4a7c15ULL);
  std::uniform_int_distribution<uint32_t> dist(0u, 0xffffffffu);
  for (auto &x : v) x = dist(rng);
}

static void make_regular_shape(std::vector<uint32_t>& shp, uint32_t M, uint64_t N_total) {
  shp.resize(M);
  uint32_t each = static_cast<uint32_t>(N_total / std::max<uint64_t>(1, M));
  for (uint32_t i = 0; i < M; ++i) shp[i] = std::max<uint32_t>(1u, each);
}

static std::vector<uint32_t> scan_exclusive_u32(const std::vector<uint32_t>& a) {
  std::vector<uint32_t> out(a.size() + 1, 0u);
  for (size_t i = 1; i < out.size(); ++i) out[i] = out[i - 1] + a[i - 1];
  return out;
}

// 0 < k <= len; vælg tilfældigt i [1, floor(2/3*len)] (som i din vens kode)
static void fill_random_ks(std::vector<uint32_t>& ks, const std::vector<uint32_t>& shp, uint64_t seed = 4242ULL) {
  size_t M = shp.size();
  ks.resize(M);
  std::mt19937_64 rng(seed);
  for (size_t i = 0; i < M; ++i) {
    uint32_t len = std::max<uint32_t>(1u, shp[i]);
    uint32_t cap = (len/3u)*2u;                  // 2/3 af segmentets længde
    if (cap == 0u) cap = 1u;
    std::uniform_int_distribution<uint32_t> d(1u, cap);
    ks[i] = d(rng);
  }
}

// -----------------------------------------------------
// Kernel: hent k'te element i hvert segment (1-indekseret)
// -----------------------------------------------------
__global__ void extract_kth_kernel(uint32_t M,
                                   const uint32_t* __restrict__ d_offsets,
                                   const uint32_t* __restrict__ ks,
                                   const uint32_t* __restrict__ sorted_keys,
                                   uint32_t* __restrict__ out)
{
  uint32_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  if (gid < M) {
    uint32_t base = d_offsets[gid];
    uint32_t k1   = ks[gid];   // 1..len
    out[gid] = sorted_keys[base + k1 - 1];
  }
}

// -----------------------------------------------------
// Enkelt “batch rank-select via sort”
// - Sorterer hvert segment med CUB SegmentedRadixSort
// - Ekstraherer k'te element pr. segment
// - Validerer både sorteringsorden og udvalgte elementer
// - Returnerer gennemsnitlig tid per kørsel i mikrosekunder
// -----------------------------------------------------
static double run_segmented_select(uint32_t M,
                                   const std::vector<uint32_t>& shp,
                                   const std::vector<uint32_t>& h_ks,
                                   const std::vector<uint32_t>& h_keys_in)
{
  // afled offsets og effektiv N fra shape
  std::vector<uint32_t> h_offsets = scan_exclusive_u32(shp);
  const uint32_t N = h_offsets.back();

  // device buffers
  uint32_t *d_keys_in=nullptr, *d_keys_out=nullptr;
  uint32_t *d_offsets=nullptr, *d_ks=nullptr, *d_out=nullptr;

  CUDA_CHECK(cudaMalloc(&d_keys_in,  sizeof(uint32_t)*N));
  CUDA_CHECK(cudaMalloc(&d_keys_out, sizeof(uint32_t)*N));
  CUDA_CHECK(cudaMalloc(&d_offsets,  sizeof(uint32_t)*(M+1)));
  CUDA_CHECK(cudaMalloc(&d_ks,       sizeof(uint32_t)*M));
  CUDA_CHECK(cudaMalloc(&d_out,      sizeof(uint32_t)*M));

  CUDA_CHECK(cudaMemcpy(d_keys_in,  h_keys_in.data(), sizeof(uint32_t)*N,   cudaMemcpyHostToDevice));
  CUDA_CHECK(cudaMemcpy(d_offsets,  h_offsets.data(), sizeof(uint32_t)*(M+1), cudaMemcpyHostToDevice));
  CUDA_CHECK(cudaMemcpy(d_ks,       h_ks.data(),      sizeof(uint32_t)*M,   cudaMemcpyHostToDevice));

  // CUB temp storage query
  void* d_temp = nullptr; size_t temp_bytes = 0;
  cub::DeviceSegmentedRadixSort::SortKeys(
      d_temp, temp_bytes,
      d_keys_in, d_keys_out,
      N, M, d_offsets, d_offsets+1,
      0, 8*sizeof(uint32_t));
  CUDA_CHECK(cudaMalloc(&d_temp, temp_bytes));

  // grid dims til extract kernel
  const uint32_t block = std::min<uint32_t>(M, 1024u);
  const uint32_t grid  = (M + block - 1) / block;

  // warm-up
  cub::DeviceSegmentedRadixSort::SortKeys(
      d_temp, temp_bytes,
      d_keys_in, d_keys_out,
      N, M, d_offsets, d_offsets+1,
      0, 8*sizeof(uint32_t));
  extract_kth_kernel<<<grid, block>>>(M, d_offsets, d_ks, d_keys_out, d_out);
  CUDA_CHECK(cudaDeviceSynchronize());

  // måling (CUDA events via GpuTimer → ms; konverter til µs)
  const int kIters = 10;
  GpuTimer timer;
  timer.start();
  for (int it=0; it<kIters; ++it) {
    cub::DeviceSegmentedRadixSort::SortKeys(
        d_temp, temp_bytes,
        d_keys_in, d_keys_out,
        N, M, d_offsets, d_offsets+1,
        0, 8*sizeof(uint32_t));
    extract_kth_kernel<<<grid, block>>>(M, d_offsets, d_ks, d_keys_out, d_out);
  }
  float ms = timer.stop();
  double us_per_iter = (ms * 1000.0) / std::max(1, kIters);

  // validering: sorteret pr. segment + korrekt k’te element
  std::vector<uint32_t> h_sorted(N), h_res(M);
  CUDA_CHECK(cudaMemcpy(h_sorted.data(), d_keys_out, sizeof(uint32_t)*N, cudaMemcpyDeviceToHost));
  CUDA_CHECK(cudaMemcpy(h_res.data(),    d_out,      sizeof(uint32_t)*M, cudaMemcpyDeviceToHost));

  for (uint32_t j=0; j<M; ++j) {
    uint32_t s = h_offsets[j], e = h_offsets[j+1];
    for (uint32_t i=s+1; i<e; ++i) {
      if (h_sorted[i-1] > h_sorted[i]) {
        printf("INVALID RESULT at i=%u (A[i-1]=%u > A[i]=%u)\n", i, h_sorted[i-1], h_sorted[i]);
        // cleanup
        cudaFree(d_temp); cudaFree(d_keys_in); cudaFree(d_keys_out);
        cudaFree(d_offsets); cudaFree(d_ks); cudaFree(d_out);
        return us_per_iter;
      }
    }
    uint32_t kth_idx = s + (h_ks[j] - 1);
    if (h_res[j] != h_sorted[kth_idx]) {
      printf("INVALID RESULT at seg=%u (res=%u != ref=%u)\n", j, h_res[j], h_sorted[kth_idx]);
      cudaFree(d_temp); cudaFree(d_keys_in); cudaFree(d_keys_out);
      cudaFree(d_offsets); cudaFree(d_ks); cudaFree(d_out);
      return us_per_iter;
    }
  }
  printf("!!!VALID RESULT!!!\n");

  // cleanup
  cudaFree(d_temp); cudaFree(d_keys_in); cudaFree(d_keys_out);
  cudaFree(d_offsets); cudaFree(d_ks); cudaFree(d_out);
  return us_per_iter;
}

// -----------------------------------------------------
// main: matcher din vens CLI og printformat
//   Usage: ./test-rank-search-k <N_total> <M_segments>
// -----------------------------------------------------
int main(int argc, char** argv) {
  if (argc != 3) {
    std::fprintf(stderr, "Usage: %s <size-of-flat-array> <size-of-shp>\n", argv[0]);
    return 1;
  }
  uint64_t N_total = std::strtoull(argv[1], nullptr, 10);
  uint32_t M       = static_cast<uint32_t>(std::strtoull(argv[2], nullptr, 10));
  if (M == 0 || N_total == 0) {
    std::fprintf(stderr, "Both N and M must be positive.\n");
    return 1;
  }

  // lav regular shape der summer til N_eff = M * floor(N_total/M)
  // (samme idé som vens “regularShape” → ens segmentlængder)
  std::vector<uint32_t> shp;
  make_regular_shape(shp, M, N_total);
  std::vector<uint32_t> offs = scan_exclusive_u32(shp);
  uint32_t N_eff = offs.back();

  // host data
  std::vector<uint32_t> h_keys(N_eff);
  std::vector<uint32_t> h_ks;
  fill_random_u32(h_keys, 12345ULL);
  fill_random_ks(h_ks, shp, 777ULL);

  // banner
  std::printf("Batch Rank Search K for N=%u and M=%u\n", N_eff, M);

  // kør
  double us = run_segmented_select(M, shp, h_ks, h_keys);
  std::printf("Runs in: %.2f us\n", us);
  return 0;
}