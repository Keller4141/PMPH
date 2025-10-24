import "flatten_rank_search_k"

-- Bench Unoptimized
-- ==
-- entry: benchUnoptimized
-- random input { [1000][6250]f32 }
-- random input { [1000][12500]f32 }
-- random input { [1000][25000]f32 }
-- random input { [1000][50000]f32 }
-- random input { [1000][100000]f32 }
-- random input { [1000][200000]f32 }
-- random input { [1000][400000]f32 }
entry benchUnoptimized [m][n] (A2d : [m][n]f32) : [m]f32 =
  let n_i32 : i32   = i32.i64 n
  let flatA         = flatten A2d
  let shp  : [m]i32 = replicate m n_i32
  let half : i32    = n_i32 / 2
  let k_val : i32   = if half < 1 then 1 else half
  let ks   : [m]i32 = replicate m k_val
  -- Build II1 as i64 (this variant expects i64 indices):
  let II1_i64 : []i64 =
    flatten (map (\(i:i64) -> replicate n i) (iota m))
  in RankSearchK.humanReasoningBatchRankSearch ks flatA shp II1_i64


-- Bench Optimized
-- ==
-- entry: benchOptimized
-- random input { [1000][6250]f32 }
-- random input { [1000][12500]f32 }
-- random input { [1000][25000]f32 }
-- random input { [1000][50000]f32 }
-- random input { [1000][100000]f32 }
-- random input { [1000][200000]f32 }
-- random input { [1000][400000]f32 }
entry benchOptimized [m][n] (A2d : [m][n]f32) : [m]f32 =
  let n_i32 : i32   = i32.i64 n
  let flatA         = flatten A2d
  let shp  : [m]i32 = replicate m n_i32
  let half : i32    = n_i32 / 2
  let k_val : i32   = if half < 1 then 1 else half
  let ks   : [m]i32 = replicate m k_val
  -- Build II1 in i64 for reduction…
  let II1_i64 : []i64 =
    flatten (map (\(i:i64) -> replicate n i) (iota m))
  -- …and convert a copy to i32 for the optimized kernel (which expects i32):
  let II1_i32 : []i32 = map i32.i64 II1_i64
  -- Segment means as pivots:
  let sums : [m]f32 =
    reduce_by_index (replicate m 0f32) (+) 0f32 II1_i64 flatA
  let ps : [m]f32 = map2 (\s len -> s / f32.i32 len) sums shp
  in RankSearchK.humanReasoningBatchRankSearchOptimized ps ks shp II1_i32 flatA


-- Bench Compiler-flattened
-- ==
-- entry: benchCompilerFlattened
-- random input { [4][16]f32 }
entry benchCompilerFlattened [m][n] (A2d : [m][n]f32) : [m]f32 =
  let n_i32 : i32   = i32.i64 n
  let flatA         = flatten A2d
  let shp  : [m]i32 = replicate m n_i32
  let half : i32    = n_i32 / 2
  let k_val : i32   = if half < 1 then 1 else half
  let ks   : [m]i32 = replicate m k_val
  in RankSearchK.compilerThinkingBatchRankSearch ks flatA shp