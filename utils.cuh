#ifndef RANKSEARCH_UTILS_CUH
#define RANKSEARCH_UTILS_CUH

#include <cstdio>
#include <cstdlib>
#include <cstdint>
#include <cassert>
#include <iostream>
#include <sys/time.h>
#include <cuda_runtime.h>

// Hvor mange gentagelser vi måler over (bruges kun hvis din .cu gør det)
#ifndef GPU_RUNS
#define GPU_RUNS 400
#endif

// ------------------------------
// CPU-tidsmåling (gettimeofday)
// ------------------------------
inline int diff_time(struct timeval* result,
                     const struct timeval* t2,
                     const struct timeval* t1)
{
    const long RES = 1000000; // mikrosekunder/sek
    long delta = (t2->tv_usec + RES * t2->tv_sec)
               - (t1->tv_usec + RES * t1->tv_sec);
    result->tv_sec  = delta / RES;
    result->tv_usec = delta % RES;
    return (delta < 0);
}

// ------------------------------
// CUDA error helpers
// ------------------------------
inline void cudaAssertCall(cudaError_t code, const char* file, int line, bool abort = true)
{
    if (code != cudaSuccess) {
        std::cerr << "[CUDA ERROR] " << cudaGetErrorString(code)
                  << " at " << file << ":" << line << std::endl;
        if (abort) std::exit(code);
    }
}

#define cudaCheck(x)     cudaAssertCall((x), __FILE__, __LINE__)
#define cudaCheckLast()  do { \
  cudaError_t _e = cudaGetLastError(); \
  if (_e != cudaSuccess) { \
    std::cerr << "[CUDA KERNEL ERROR] " << cudaGetErrorString(_e) << std::endl; \
    std::exit(1); \
  } \
} while(0)

// GPU event-timer (ms)
struct GpuTimer {
    cudaEvent_t start_, stop_;
    GpuTimer()  { cudaEventCreate(&start_); cudaEventCreate(&stop_); }
    ~GpuTimer() { cudaEventDestroy(start_); cudaEventDestroy(stop_); }
    void start() { cudaEventRecord(start_); }
    float stop() {
        cudaEventRecord(stop_); cudaEventSynchronize(stop_);
        float ms = 0.f; cudaEventElapsedTime(&ms, start_, stop_); return ms;
    }
};

// Små utils
inline uint32_t ceil_log2(uint32_t x) {
    if (x == 0) { std::fprintf(stderr, "ceil_log2(0) is undefined.\n"); std::exit(1); }
    uint32_t r = 0, p = 1; while (p < x) { p <<= 1; ++r; } return r;
}
inline void write_runtime_to_file(const char* filename, double microseconds) {
    FILE* f = std::fopen(filename, "w");
    assert(f && "write_runtime_to_file: cannot open file");
    std::fprintf(f, "%f", microseconds);
    std::fclose(f);
}

// ---------- Compatibility aliases (så din .cu kan bruge gamle navne) ----------
#ifndef cudaSucceeded
#define cudaSucceeded(x) cudaCheck(x)
#endif
#ifndef cudaCheckError
#define cudaCheckError() cudaCheckLast()
#endif
#ifndef CUDA_CHECK
#define CUDA_CHECK(x) cudaCheck(x)
#endif

#endif // RANKSEARCH_UTILS_CUH