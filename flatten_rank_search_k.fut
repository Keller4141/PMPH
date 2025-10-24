import "lib/github.com/diku-dk/sorts/radix_sort"

-- ========== small helpers ==========

-- Eksklusiv scan via simpel prefix-map.
let scanex [n] 't (op: t -> t -> t) (ne: t) (xs: [n]t) : [n]t =
  scan op ne (map (\i -> if i == 0 then ne else xs[i-1]) (iota n))

-- Segmenteret inklusiv scan: 'true' i flags starter nyt segment.
let sgm_scan_inc [n] 't
  (op: t -> t -> t) (ne: t)
  (flags: [n]bool) (arr: [n]t) : [n]t =
  let scanned : [n](bool,t) =
    scan (\(f_acc, acc) (f_cur, v) ->
            let f' = f_acc || f_cur
            let v' = if f_cur then v else op acc v
            in (f', v'))
         (false, ne)
         (zip flags arr)
  in map (\(_, v) -> v) scanned

-- Udvider én værdi pr. segment til flad længde (samme n som data).
let expand_val_per_seg [m] 't (shp: [m]i32) (vals: [m]t) : []t =
  let total_i32 : i32 = reduce (+) 0 shp
  let n : i64 = i64.i32 total_i32
  let offs  : [m]i32 = scanex (+) 0 shp
  let idxs  : [m]i64 = map i64.i32 offs
  let base  : [n]t   = scatter (replicate n vals[0]) idxs vals
  let starts_i32 : [n]i32 = scatter (replicate n 0) idxs (replicate m 1)
  let flags : [n]bool = map (\x -> x == 1) starts_i32
  in sgm_scan_inc (\_ y -> y) vals[0] flags base

-- Udvider i32 pr. segment via segmenteret plusscan.
let expand_i32_by_seg [m] (shp: [m]i32) (vals: [m]i32) : []i32 =
  let total_i32 : i32 = reduce (+) 0 shp
  let n : i64 = i64.i32 total_i32
  let offs  : [m]i32 = scanex (+) 0 shp
  let idxs  : [m]i64 = map i64.i32 offs
  let base  : [n]i32 = scatter (replicate n 0) idxs vals
  let starts_i32 : [n]i32 = scatter (replicate n 0) idxs (replicate m 1)
  let flags : [n]bool = map (\x -> x == 1) starts_i32
  in sgm_scan_inc (+) 0 flags base

-- True på første element i hvert segment (samme flade længde som data).
let seg_start_flags [m] (shp: [m]i32) : []bool =
  let total_i32 : i32 = reduce (+) 0 shp
  let n : i64 = i64.i32 total_i32
  let offs  : [m]i32 = scanex (+) 0 shp
  let idxs  : [m]i64 = map i64.i32 offs
  let marks : [n]i32 = scatter (replicate n 0) idxs (replicate m 1)
  in map (\x -> x == 1) marks

-- ========== trevejspartition (til compilerThinking) ==========

module Partition3 = {
  let batch [m] [n]
      (p1: f32 -> f32 -> bool) (p2: f32 -> f32 -> bool)
      (A: [n]f32) (shp: [m]i32) (pivots: [m]f32)
    : ([n]f32, ([m]i32, [m]i32)) =

    -- Pivots udvides til flad længde n
    let piv_e : [n]f32 = (expand_val_per_seg shp pivots) :> [n]f32

    -- Klassifikation pr. element
    let lt_b  : [n]bool = map2 p1 A piv_e
    let eq_b  : [n]bool = map2 p2 A piv_e
    let lt_i  : [n]i32  = map (\b -> if b then 1 else 0) lt_b
    let eq_i  : [n]i32  = map (\b -> if b then 1 else 0) eq_b
    let gt_i  : [n]i32  = map2 (\l e -> if l==1 || e==1 then 0 else 1) lt_i eq_i

    -- Segmenteret prefix for hver klasse
    let flags_n : [n]bool = (seg_start_flags shp) :> [n]bool
    let lt_ps : [n]i32 = sgm_scan_inc (+) 0 flags_n lt_i
    let eq_ps : [n]i32 = sgm_scan_inc (+) 0 flags_n eq_i
    let gt_ps : [n]i32 = sgm_scan_inc (+) 0 flags_n gt_i

    -- Optælling pr. segment (sidste prefix-værdi i segmentet)
    let offs  : [m]i32 = scanex (+) 0 shp
    let cnt_lt : [m]i32 =
      map2 (\o len -> if len == 0 then 0 else lt_ps[o + len - 1]) offs shp
    let cnt_eq : [m]i32 =
      map2 (\o len -> if len == 0 then 0 else eq_ps[o + len - 1]) offs shp

    -- Startoffsets for hver region i det nye layout
    let lt_off : [m]i32 = offs
    let eq_off : [m]i32 = map2 (+) offs cnt_lt
    let gt_off : [m]i32 = map3 (\o a b -> o + a + b) offs cnt_lt cnt_eq

    -- Udvid offsets til fladt [n]
    let lt_off_e : [n]i32 = (expand_i32_by_seg shp lt_off) :> [n]i32
    let eq_off_e : [n]i32 = (expand_i32_by_seg shp eq_off) :> [n]i32
    let gt_off_e : [n]i32 = (expand_i32_by_seg shp gt_off) :> [n]i32

    -- Beregn position pr. element uden iota/map6 – zips matcher automatisk [n].
    let z1 : [n](i32,i32,i32,i32,i32) = zip5 lt_i eq_i lt_ps eq_ps gt_ps
    let z2 : [n](i32,i32,i32)         = zip3 lt_off_e eq_off_e gt_off_e
    let z  : [n]((i32,i32,i32,i32,i32),(i32,i32,i32)) = zip z1 z2

    let pos2 : [n]i32 =
      map (\((l,e,lp,ep,gp),(lo,eo,go)) ->
             if l == 1 then lo + (lp - 1)
             else if e == 1 then eo + (ep - 1)
             else               go + (gp - 1))
          z

    -- Scatter til ny rækkefølge. VIGTIGT: allokér med n (ikke total).
    let A' : [n]f32 = scatter (replicate n A[0]) (map i64.i32 pos2) A

    in (A', (cnt_lt, cnt_eq))
}

-- ========== RankSearchK ==========

module RankSearchK = {
  -- Baseline: radix-sort hvert segment og vælg k'te (1-indekseret).
  let radixSortRankSearchBatch [m] [n]
      (ks: [m]i32) (shp: [m]i32) (A: [n]f32) : [m]f32 =
    let starts_i32 : [m]i32 = scanex (+) 0 shp
    let starts     : [m]i64 = map i64.i32 starts_i32
    let lens64     : [m]i64 = map i64.i32 shp
    let select_sorted (k: i32) (xs: []f32) : f32 =
      let srt = radix_sort f32.num_bits f32.get_bit xs
      in srt[k-1]
    in map3
         (\k len64 st64 ->
            let seg : []f32 = map (\j -> A[st64 + j]) (iota len64)
            in select_sorted k seg)
         ks lens64 starts

  -- Opt-varianten med eksplicitte pivots.
  let humanReasoningBatchRankSearchOptimized [m] [n]
      (piv0 : [m]f32) (k0: [m]i32) (shp0: [m]i32)
      (II1: *[n]i32) (A0: [n]f32) : [m]f32 =
    let ans0 = replicate m 0f32
    let (_,_,_,_,_,ans) =
      loop (k_run, shp_run, seg, arr, piv, ans) =
           (k0,   shp0,    II1, A0,  piv0, ans0)
      while (length arr > 0) do
        -- seg bruges til at slå pivot pr. element op: cast til i64 først.
        let seg64 : []i64 = map i64.i32 seg
        let piv_fl : []f32 = map (\i -> piv[i]) seg64

        let lt_i32 : []i32 = map2 (\x p -> if x < p  then 1 else 0) arr piv_fl
        let eq_i32 : []i32 = map2 (\x p -> if x == p then 1 else 0) arr piv_fl

        let cnt_lt : [m]i32 = reduce_by_index (replicate m 0) (+) 0 seg64 lt_i32
        let cnt_eq : [m]i32 = reduce_by_index (replicate m 0) (+) 0 seg64 eq_i32

        let choice : [m]i8 =
          map3 (\k l e ->
                  if k == -1 then -1i8
                  else if k <= l then 0i8
                  else if k <= l + e then 1i8
                  else 2i8)
               k_run cnt_lt cnt_eq

        let k_next : [m]i32 =
          map4 (\ch k l e ->
                  if ch == -1i8 then -1
                  else if ch ==  0i8 then k
                  else if ch ==  1i8 then -1
                  else k - l - e)
               choice k_run cnt_lt cnt_eq

        let shp_next : [m]i32 =
          map4 (\ch l e len ->
                  if ch == -1i8 then 0
                  else if ch ==  0i8 then l
                  else if ch ==  1i8 then 0
                  else len - l - e)
               choice cnt_lt cnt_eq shp_run

        -- skriv færdige pivots ind i resultatet
        let triples       = zip3 choice (iota m) piv      -- (i8, i64, f32)
        let done_tr       = filter (\(ch, _, _) -> ch == 1i8) triples
        let idx_done : []i64 = map (\(_,i,_) -> i) done_tr
        let val_done : []f32 = map (\(_,_,p) -> p) done_tr
        let ans_next = scatter ans idx_done val_done

        -- behold kun de elementer der fortsat er “in play”
        let keep =
          zip4 lt_i32 eq_i32 arr seg
          |> filter (\(l,e,_,i) ->
                       let ch = choice[i] in
                       if ch == 0i8 then l == 1
                       else if ch == 2i8 then (l == 0) && (e == 0)
                       else false)
        let (_, _, arr_next, seg_next) = unzip4 keep

        -- nye pivots: sidste element i hvert (nye) segment; cast til i64 for index
        let offs_next : [m]i32 = scanex (+) 0 shp_next
        let piv_next  : [m]f32 =
          map3 (\off len p_old ->
                  if len > 0
                  then arr_next[i64.i32 off + i64.i32 len - 1]
                  else p_old)
               offs_next shp_next piv

        in (k_next, shp_next, seg_next, arr_next, piv_next, ans_next)
    in ans


  -- Pivot = sidste element i segmentet (som i din oprindelige vennekode)
  let humanReasoningBatchRankSearch [n] [m]
      (k0: [m]i32) (A: [n]f32) (shp0: [m]i32) (II1: [n]i64) : [m]f32 =
    let (_, _, _, _, acc) =
      loop (k_run, arr, shp_run, seg64, acc) =
           (k0,    A,   shp0,   II1,   replicate m f32.nan)
      while (length arr > 0) do
        let offs = scanex (+) 0 shp_run
        let pivs =
          map3 (\off len cur ->
                  if len > 0 then arr[off + len - 1] else cur)
               offs shp_run acc

        let seg32 : []i32 = map i32.i64 seg64
        let piv_fl = map (\i -> pivs[i]) seg32

        let lt_i32 = map2 (\x p -> if x < p then 1 else 0) arr piv_fl
        let eq_i32 = map2 (\x p -> if x == p then 1 else 0) arr piv_fl

        let cnt_lt : [m]i32 = reduce_by_index (replicate m 0) (+) 0 seg64 lt_i32
        let cnt_eq : [m]i32 = reduce_by_index (replicate m 0) (+) 0 seg64 eq_i32

        let pick : [m]i8 =
          map3 (\k l e ->
                  if k == -1 then -1i8
                  else if k <= l then 0i8
                  else if k <= l + e then 1i8
                  else 2i8)
               k_run cnt_lt cnt_eq

        let k_next =
          map4 (\c k l e ->
                  if c == -1i8 then -1
                  else if c ==  0i8 then k
                  else if c ==  1i8 then -1
                  else k - l - e)
               pick k_run cnt_lt cnt_eq

        let acc_next =
          map3 (\c p cur -> if c == 1i8 then p else cur) pick pivs acc

        let shp_next =
          map4 (\c l e len ->
                  if c == -1i8 then 0
                  else if c ==  0i8 then l
                  else if c ==  1i8 then 0
                  else len - l - e)
               pick cnt_lt cnt_eq shp_run

        let keep =
          zip4 lt_i32 eq_i32 arr seg64
          |> filter (\(l,e,_,i) ->
                       let c = pick[i] in
                       if c == -1i8 then false
                       else if c ==  0i8 then l == 1
                       else if c ==  1i8 then false
                       else l == 0 && e == 0)
        let (_, _, arr_next, seg_next) = unzip4 keep

        in (k_next, arr_next, shp_next, seg_next, acc_next)
    in acc

-- Compiler-thinking: fysisk trevejspartition for hver iteration
let compilerThinkingBatchRankSearch [m] [n]
    (ks: [m]i32) (A: [n]f32) (shp: [m]i32) : [m]f32 =
  let (_,_,_, res) =
    loop (k_run, arr, shp_run, out) =
         (ks, A, shp, replicate m f32.nan)
    while (reduce (+) 0 shp_run) > 0 do
      let offs = scanex (+) 0 shp_run
      let pivs =
        map3 (\o len r -> if len > 0 then arr[o + len - 1] else r)
             offs shp_run out

      let (arr_part, (cnt_lt, cnt_eq)) =
        Partition3.batch (<) (==) arr shp_run pivs

      let k_next =
        map4 (\k l e len ->
                if k <= l && len > 0 then k
                else if k <= l + e || len == 0 then k
                else k - l - e)
             k_run cnt_lt cnt_eq shp_run

      let out_next =
        map5 (\k (l,e) len p r ->
               if k <= l && len > 0 then r
               else if k <= l + e || len == 0 then p
               else r)
             k_run (zip cnt_lt cnt_eq) shp_run pivs out

      let shp_next =
        map4 (\k l e len ->
                if k <= l && len > 0 then l
                else if k <= l + e || len == 0 then 0
                else len - l - e)
             k_run cnt_lt cnt_eq shp_run

      -- Bind en fælles størrelsesvariabel p til ALLE flade, udvidede arrays
      let old_off_e =
        scanex (+) 0 shp_run
        |> expand_i32_by_seg shp_next
        |> map i64.i32        -- : []i64

      let new_off_e =
        scanex (+) 0 shp_next
        |> expand_i32_by_seg shp_next
        |> map i64.i32        -- : []i64

      let k_e = expand_i32_by_seg shp_next k_run    |> map i64.i32  -- : []i64
      let l_e = expand_i32_by_seg shp_next cnt_lt   |> map i64.i32  -- : []i64
      let e_e = expand_i32_by_seg shp_next cnt_eq   |> map i64.i32  -- : []i64

      -- Brug længden af new_off_e som fælles "p".
      let p : i64 = length new_off_e

      -- Rekonstruér arr_next ved at indeksere alle arrays ved samme i \in [0..p).
      let arr_next : []f32 =
        map (\i ->
               let i' = i - new_off_e[i]
               in if k_e[i] <= l_e[i]
                  then arr_part[old_off_e[i] + i']
                  else arr_part[old_off_e[i] + i' + l_e[i] + e_e[i]])
            (iota p)

      in (k_next, arr_next, shp_next, out_next)
  in res
}