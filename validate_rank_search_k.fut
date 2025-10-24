-- validate_rank_search_k.fut (reworked)
-- Tjekker tre varianter mod en “sandhed”: radix-sort pr. segment + vælg k.

import "rank_search_k"

-- Små hjælpere til at bygge flad batch-repræsentation ud fra et 2D input.
let flatten2d [m][n] 't (x: [m][n]t) : []t =
  concat (map (\row -> row) x)

let mk_shape_and_ks [m] (seg_len: i32) : ([m]i32, [m]i32) =
  let shp = replicate m seg_len
  let ks  = map (\ln -> max 1 ((ln+1)/2)) shp
  in (shp, ks)

let mk_ii1_i32 [m] (shp: [m]i32) : []i32 =
  concat (map (\seg -> replicate shp[seg] seg) (iota m))

-- =========================================
-- 1) Unoptimized (human reasoning, pivot fra data)
-- =========================================

-- ==
-- entry: validationUnoptimized
-- input { [10000][10]f32.random }
-- output { true }
-- input { [1000][100]f32.random }
-- output { true }
entry validationUnoptimized [m][n] (A2d: [m][n]f32) : bool =
  let seg_len : i32 = i32 n
  let flatA         = flatten2d A2d
  let (shp, ks)     = mk_shape_and_ks m seg_len
  let II1_i64       = map i64.i32 (mk_ii1_i32 m shp)

  let oracle = RankSearchK.radixSortRankSearchBatch ks shp flatA
  let trial  = RankSearchK.humanReasoningBatchRankSearch ks flatA shp II1_i64

  in reduce (&&) true (map2 (==) oracle trial)

-- =========================================
-- 2) Optimized (human reasoning, pivot-estimat = gennemsnit pr. segment)
-- =========================================

-- ==
-- entry: validationOptimized
-- input { [10000][10]f32.random }
-- output { true }
-- input { [1000][100]f32.random }
-- output { true }
entry validationOptimized [m][n] (A2d: [m][n]f32) : bool =
  let seg_len : i32 = i32 n
  let flatA         = flatten2d A2d
  let (shp, ks)     = mk_shape_and_ks m seg_len
  let II1_i32       = mk_ii1_i32 m shp
  let II1_i64       = map i64.i32 II1_i32

  -- mean pr. segment som pivot-estimat
  let sums : [m]f32 =
    reduce_by_index m II1_i32 flatA 0f32 (+)
  let ps : [m]f32 =
    map2 (\s ln -> s / f32 ln) sums shp

  let oracle = RankSearchK.radixSortRankSearchBatch ks shp flatA
  let trial  = RankSearchK.humanReasoningBatchRankSearchOptimized ps ks shp II1_i64 flatA

  in reduce (&&) true (map2 (==) oracle trial)

-- =========================================
-- 3) “Compiler-flattened” reference (partition3-baseret)
-- =========================================

-- ==
-- entry: validationCompilerFlattened
-- input { [10000][10]f32.random }
-- output { true }
-- input { [1000][100]f32.random }
-- output { true }
entry validationCompilerFlattened [m][n] (A2d: [m][n]f32) : bool =
  let seg_len : i32 = i32 n
  let flatA         = flatten2d A2d
  let (shp, ks)     = mk_shape_and_ks m seg_len

  let oracle = RankSearchK.radixSortRankSearchBatch ks shp flatA
  let trial  = RankSearchK.compilerThinkingBatchRankSearch ks flatA shp

  in reduce (&&) true (map2 (==) oracle trial)