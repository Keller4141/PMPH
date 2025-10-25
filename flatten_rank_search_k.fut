import "lib/github.com/diku-dk/sorts/radix_sort"

-- =======================
-- Små byggesten
-- =======================

let mkFlagArray [m] 't
                (aoa_shp : [m]i32) (zero : t)
                (aoa_val : [m]t) : []t =
  let shp_rot  = map (\i -> if i == 0 then 0 else aoa_shp[i-1]) (iota m)
  let shp_scan = scan (+) 0 shp_rot
  let aoa_len  = if m == 0 then 0 else shp_scan[m-1] + aoa_shp[m-1]
  let shp_ind  = map2 (\size ind ->
                         if size == 0 then -1 else i64.i32 ind)
                      aoa_shp shp_scan
  in scatter (replicate (i64.i32 aoa_len) zero) shp_ind aoa_val

let scanex [n] 't (op: t -> t -> t) (ne: t) (arr: [n]t) : [n]t =
  scan op ne (map (\i -> if i == 0 then ne else arr[i-1]) (iota n))

let sgmScanInc [n] 't (op: t -> t -> t) (ne: t)
                      (flags: [n]bool) (arr: [n]t) : [n]t =
  let (_, res) =
    unzip <| scan (\(x_flag,x) (y_flag,y) ->
                     let fl = x_flag || y_flag
                     let vl = if y_flag then y else op x y
                     in (fl, vl))
                  (false, ne)
                  (zip flags arr)
  in res

let expand [m] 't (plus : t -> t -> t) (t_zero : t)
                  (shp : [m]i32) (vals : [m]t) : []t =
  let (flag_size, flag_val) =
    zip shp vals |> mkFlagArray shp (0i32, t_zero) |> unzip
  in sgmScanInc plus t_zero (map (!= 0i32) flag_size) flag_val

let expandi32  = expand (+) 0i32
let expandi64  = expand (+) 0i64
let expandf32  = expand (+) 0f32
let expandbool = expand (||) false

-- Returns an unsized []f32 so we avoid named-size conflicts like n vs n64.
let expand_val_per_seg [m] (shp: [m]i32) (vals: [m]f32) : []f32 =
  let total : i32   = reduce (+) 0 shp
  let offs  : [m]i32 = scanex (+) 0 shp
  let idxs  : [m]i64 = map i64.i32 offs
  -- base: initialiser hele fladen med vals[0], og skriv segment-startværdier ind
  let base  : []f32  = scatter (replicate (i64.i32 total) vals[0]) idxs vals
  -- flags: true ved segment-starts
  let marks : []i32  = scatter (replicate (i64.i32 total) 0) idxs (replicate m 1)
  let flags : []bool = map (\x -> x == 1) marks
  in sgmScanInc (\_ y -> y) vals[0] flags base

-- Returnerer en usized []i32, så vi undgår navngivne størrelser.
let expand_i32_by_seg [m] (shp: [m]i32) (vals: [m]i32) : []i32 =
  let total : i32 = reduce (+) 0 shp
  let offs  : [m]i32 = scanex (+) 0 shp
  let idxs  : [m]i64 = map i64.i32 offs
  let base  : []i32  = scatter (replicate (i64.i32 total) 0) idxs vals
  let marks : []i32  = scatter (replicate (i64.i32 total) 0) idxs (replicate m 1)
  let flags : []bool = map (\x -> x == 1) marks
  in sgmScanInc (+) 0 flags base

let seg_start_flags [m] (shp: [m]i32) : []bool =
  let total : i32 = reduce (+) 0 shp
  let offs  : [m]i32 = scanex (+) 0 shp
  let idxs  : [m]i64 = map i64.i32 offs
  let marks : []i32  = scatter (replicate (i64.i32 total) 0) idxs (replicate m 1)
  in map (\x -> x == 1) marks

-- =======================
-- 3-vejs partition pr. segment
-- =======================
module Partition3 = {
  let batch [m] [n]
      (lt: f32 -> f32 -> bool) (eq: f32 -> f32 -> bool)
      (xs: [n]f32) (shp: [m]i32) (pivs: [m]f32)
    : ([n]f32, ([m]i32, [m]i32)) =

    -- Udvid pivot pr. segment.
    let piv_e : [n]f32 = expandf32 shp pivs :> [n]f32

    -- Klassifikation pr. element.
    let c_lt : [n]bool = map2 lt xs piv_e
    let c_eq : [n]bool = map2 eq xs piv_e
    let b2i = (\b -> if b then 1 else 0)
    let t_lt : [n]i32 = map b2i c_lt
    let t_eq : [n]i32 = map b2i c_eq
    let t_gt : [n]i32 = map2 (\a b -> if a || b then 0 else 1) c_lt c_eq

    -- Segment-start flags.
    let flags : [n]bool = mkFlagArray shp false (replicate m true) :> [n]bool

    -- Segmenterede inkl. præfikser (1-baserede) for hver klasse.
    let ps_lt  : [n]i32 = sgmScanInc (+) 0 flags t_lt
    let ps_eq0 : [n]i32 = sgmScanInc (+) 0 flags t_eq   -- <- UDEN base!
    let ps_gt  : [n]i32 = sgmScanInc (+) 0 flags t_gt

    -- Sidste indeks i hvert segment samt offsets.
    let offs   : [m]i32 = scanex (+) 0 shp
    let lastix : [m]i32 = map (\len -> len-1) shp

    -- Antal pr. segment.
    let cnt_lt : [m]i32 = map2 (\o i -> if i == -1 then 0 else ps_lt[o+i])  offs   lastix
    let cnt_eq : [m]i32 = map2 (\o i -> if i == -1 then 0 else ps_eq0[o+i]) offs   lastix

    -- Startoffsets for hver klasse.
    let off_lt : [m]i32 = offs
    let off_eq : [m]i32 = map2 (+) offs cnt_lt
    let off_gt : [m]i32 = map3 (\o a b -> o + a + b) offs cnt_lt cnt_eq

    -- Udvid offsets til fladt layout.
    let off_lt_e : [n]i32 = expandi32 shp off_lt :> [n]i32
    let off_eq_e : [n]i32 = expandi32 shp off_eq :> [n]i32
    let off_gt_e : [n]i32 = expandi32 shp off_gt :> [n]i32

    -- Absolutte destinationer (0-baseret): offset + (prefix-1).
    let pos_lt : [n]i32 = map2 (\o p -> o + (p - 1)) off_lt_e ps_lt
    let pos_eq : [n]i32 = map2 (\o p -> o + (p - 1)) off_eq_e ps_eq0   -- <- fix
    let pos_gt : [n]i32 = map2 (\o p -> o + (p - 1)) off_gt_e ps_gt

    -- Vælg endelig destination pr. element.
    let dest : [n]i32 =
      map3 (\bl be (a,b,c) -> if bl then a else if be then b else c)
           c_lt c_eq (zip3 pos_lt pos_eq pos_gt)

    -- Scatter i ny rækkefølge.
    let xs' : [n]f32 = scatter (copy xs) (map i64.i32 dest) xs

    in (xs', (cnt_lt, cnt_eq))
}

-- =======================
-- RankSearchK
-- =======================
module RankSearchK = {

  let radixSortRankSearchBatch [m] [n]
      (ks: [m]i32) (shp: [m]i32) (A: [n]f32) : [m]f32 =
    let starts : [m]i32 = scanex (+) 0 shp
    let select_sorted (k: i32) (xs: []f32) : f32 =
      let srt = radix_sort f32.num_bits f32.get_bit xs
      in srt[k-1]
    in map3 (\k len st ->
               let seg = map (\i -> A[i64.i32 st + i]) (iota (i64.i32 len))
               in select_sorted k seg)
            ks shp starts

  -- Generaliseret (bruges til opt-varianten med forudgivne pivots)
  let generalizedHumanReasoningBatchRankSearchOptimized [m] [n] 't
      (lth : t -> t -> bool) (eq : t -> t -> bool) (zero : t)
      (ps : [m]t) (ks: [m]i32) (shp: [m]i32)
      (II1: *[n]i32) (A: [n]t) : [m]t =
    let result0 = replicate m zero
    let (_,_,_,_,_,res) =
      loop (ks, ps, shp, II1, A, result) = (ks, ps, shp, II1, A, result0)
      while (length A > 0) do
        let ps_expanded = map (\i -> ps[i]) II1
        let flags = map2 (\x p ->
                            if lth x p then (true,false)
                            else if eq x p then (false,true)
                            else (false,false)) A ps_expanded
        let (cnt_lth, cnt_eq) =
          map (\(a,b) -> (i32.bool a, i32.bool b)) flags
          |> reduce_by_index (replicate m (0,0))
                             (\(a,b) (c,d) -> (a+c, b+d))
                             (0,0)
                             (map i64.i32 II1)
          |> unzip

        let kinds =
          map3 (\k l e ->
                  if k == -1 then -1i8
                  else if k <= l then 0i8
                  else if k <= l+e then 1i8
                  else 2i8) ks cnt_lth cnt_eq

        let (shp', ks') =
          map5 (\len k kind l e ->
                  if kind == -1i8 then (len, -1)
                  else if kind == 0i8 then (l, k)
                  else if kind == 1i8 then (0, -1)
                  else (len - (l+e), k - (l+e)))
               shp ks kinds cnt_lth cnt_eq
          |> unzip

        let done = filter (\(kd, _, _) -> kd == 1i8)
                          (zip3 kinds (iota m) ps)
        let (_, is, vs) = unzip3 done
        let result' = scatter result is vs

        let zipped = zip3 flags A II1
        let kept = filter (\((l,e),_,i) ->
                             let kd = kinds[i]
                             in if kd == -1i8 || kd == 1i8 then false
                                else if kd == 0i8 then l
                                else (not l) && (not e)) zipped
        let (_, A', II1') = unzip3 kept

        let ps' =
          map2 (\i size -> if size > 0 then A'[i-1] else zero)
               (scan (+) 0 shp') shp'

        in (ks', ps', shp', II1', A', result')
    in res

  let humanReasoningBatchRankSearchOptimized [m] [n]
      (ps : [m]f32) (ks: [m]i32) (shp: [m]i32)
      (II1: *[n]i32) (A: [n]f32) : [m]f32 =
    generalizedHumanReasoningBatchRankSearchOptimized (<) (==) 0f32 ps ks shp II1 A

  -- Pivot = sidste element i segmentet
  let humanReasoningBatchRankSearch [n] [m]
      (ks: [m]i32) (As: [n]f32)
      (shp : [m]i32) (II1 : [n]i64) : [m]f32 =
    let (_, _, _, _, results) =
      loop ((ks : [m]i32), As, (shp : [m]i32), II1, results : [m]f32) =
           (ks, As, shp, II1, replicate m f32.nan)
      while (length As > 0) do
        let ps =
          map3 (\i size result ->
                  if size > 0 then As[i-1] else result)
               (scan (+) 0 shp) shp results
        let ps_expanded = map (\i -> ps[i]) II1
        let lth = map2 (\x p -> i32.bool (x < p))  As ps_expanded
        let eqv = map2 (\x p -> i32.bool (x == p)) As ps_expanded
        let cnt_lth = reduce_by_index (replicate m 0) (+) 0 II1 lth :> [m]i32
        let cnt_eq  = reduce_by_index (replicate m 0) (+) 0 II1 eqv :> [m]i32

        let kinds =
          map3 (\k a b ->
                  if k == -1 then -1
                  else if k <= a then 0
                  else if k <= a + b then 1
                  else 2) ks cnt_lth cnt_eq

        let ks' =
          map4 (\kind k a b ->
                  if kind == -1 then -1
                  else if kind == 0 then k
                  else if kind == 1 then -1
                  else k - a - b) kinds ks cnt_lth cnt_eq

        let results' =
          map3 (\kind p r -> if kind == 1 then p else r) kinds ps results

        let shp' =
          map4 (\kind a b sz ->
                  if kind == -1 then 0
                  else if kind == 0 then a
                  else if kind == 1 then 0
                  else sz - a - b) kinds cnt_lth cnt_eq shp

        let kept =
          zip4 lth eqv As II1
          |> filter (\(l,e,_,i) ->
                       let kind = kinds[i]
                       in if kind == -1 then false
                          else if kind == 0 then l == 1
                          else if kind == 1 then false
                          else l == 0 && e == 0)
        let (_, _, As', II1') = unzip4 kept
        in (ks', As', shp', II1', results')
    in results

  -- Compiler-thinking: fysisk 3-vejspartition pr. iteration (pivot = midten)
  let compilerThinkingBatchRankSearch [m] [n]
      (ks: [m]i32) (As: [n]f32) (shp : [m]i32) : [m]f32 =
    let (_, _, _, result) =
      loop (ks, As, shp, results) =
           (ks, As, shp, replicate m f32.nan)
      while (reduce (+) 0 shp) > 0 do
        let offsets = scanex (+) 0 shp
        let ps =
          map3 (\off len r ->
                  if len > 0
                  then As[i64.i32 off + i64.i32 (len / 2)]
                  else r)
               offsets shp results

        let (As_part, (cnt_lth, cnt_eq)) = Partition3.batch (<) (==) As shp ps

        let ks' =
          map4 (\k a b sz ->
                  if k <= a && sz > 0 then k
                  else if k <= a + b || sz == 0 then k
                  else k - a - b)
               ks cnt_lth cnt_eq shp

        let results' =
          map5 (\k (a,b) sz p r ->
                  if k <= a && sz > 0 then r
                  else if k <= a + b || sz == 0 then p
                  else r)
               ks (zip cnt_lth cnt_eq) shp ps results

        let shp' =
          map4 (\k a b sz ->
                  if k <= a && sz > 0 then a
                  else if k <= a + b || sz == 0 then 0
                  else sz - a - b)
               ks cnt_lth cnt_eq shp

        let n'  = reduce (+) 0 shp' |> i64.i32
        let old_off_e =
          scanex (+) 0 shp
          |> expandi32 shp'
          |> map i64.i32 :> [n']i64
        let new_off_e =
          scanex (+) 0 shp'
          |> expandi32 shp'
          |> map i64.i32 :> [n']i64

        let ks_e = expandi32 shp' ks       |> map i64.i32 :> [n']i64
        let a_e  = expandi32 shp' cnt_lth  |> map i64.i32 :> [n']i64
        let b_e  = expandi32 shp' cnt_eq   |> map i64.i32 :> [n']i64

        let As' =
          map4 (\newi oldo newo (k,a,b) ->
                  let i = newi - newo
                  in if k <= a
                     then As_part[oldo + i]
                     else As_part[oldo + i + a + b])
               (iota n') old_off_e new_off_e (zip3 ks_e a_e b_e)

        in (ks', As', shp', results')
    in result
}