import "flatten_rank_search_k"

-- ==
-- entry: check_partition3_batch
-- input { [4f32,3.5f32,3f32,2f32,3.5f32,
--          9f32,10f32,11f32,10f32,
--          2f32,1f32,3f32]
--         [5i32,4i32,3i32]
--         [3.5f32,10f32,2f32] }
-- output { [3f32,2f32,3.5f32,3.5f32,4f32,
--           9f32,10f32,10f32,11f32,
--           1f32,2f32,3f32]
--          [2i32,1i32,1i32]
--          [2i32,2i32,1i32] }

-- ==
-- entry: check_partition3_batch
-- input { [5f32,1f32,6f32,5f32,
--          2.5f32,0f32]
--         [4i32,0i32,2i32]
--         [5f32,7f32,2.5f32] }
-- output { [1f32,5f32,5f32,6f32,
--           0f32,2.5f32]
--          [1i32,0i32,1i32]
--          [2i32,0i32,1i32] }

entry check_partition3_batch [n][m]
  (xs   : [n]f32)
  (shp  : [m]i32)
  (pivs : [m]f32)
  : ([n]f32, [m]i32, [m]i32) =
  let (xs_part, (cnt_lt, cnt_eq)) =
    Partition3.batch (<) (==) xs shp pivs
  in (xs_part, cnt_lt, cnt_eq)