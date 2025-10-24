-- Vi sender et fladt array + segmentlængder + en pivot pr. segment,
-- og forventer at få (omarrangeret array, cnt(<), cnt(==)) pr. segment tilbage.

import "rank_search_k"

-- ==
-- entry check_partition3_batch
-- input  { [9f32,34f32,1f32,23f32,3f32,4f32,4f32,10f32,15f32,2f32,   6f32,4f32,10f32,20f32,1f32,  2f32,2f32,2f32]
--          [10i32, 5i32, 3i32]
--          [4f32, 6f32, 1f32] }
-- output { [1f32,3f32,2f32,4f32,4f32,9f32,34f32,23f32,10f32,15f32,   4f32,1f32,6f32,10f32,20f32,  2f32,2f32,2f32]
--          [3i32, 2i32, 0i32]
--          [2i32, 1i32, 0i32] }

-- input  { [9f32,34f32,1f32,23f32,3f32,4f32,4f32,10f32,15f32,2f32,   6f32,4f32,10f32,20f32,1f32,  2f32,2f32,2f32]
--          [10i32, 5i32, 3i32, 0i32]
--          [4f32, 6f32, 1f32, 0f32] }
-- output { [1f32,3f32,2f32,4f32,4f32,9f32,34f32,23f32,10f32,15f32,   4f32,1f32,6f32,10f32,20f32,  2f32,2f32,2f32]
--          [3i32, 2i32, 0i32, 0i32]
--          [2i32, 1i32, 0i32, 0i32] }

entry check_partition3_batch [n][m]
  (xs   : [n]f32)
  (shp  : [m]i32)
  (pivs : [m]f32)
  : ([n]f32, [m]i32, [m]i32) =
  let (xs_part, (cnt_lt, cnt_eq)) =
    Partition3.batch (<) (==) xs shp pivs
  in (xs_part, cnt_lt, cnt_eq)