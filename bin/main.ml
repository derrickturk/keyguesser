module KeyMap = Map.Make(String)
module KeySet = Set.Make(String)

let load path =
  let load_chan chan =
    let chan' = Csv.of_channel ~has_header:true ~strip:false chan in
    let hdr = Csv.Rows.header chan' in
    let incr = function
      | None -> Some 1
      | Some n -> Some (n + 1)
    in
    let init = List.map (fun h -> (h, KeyMap.empty)) hdr in
    let f so_far row = List.map2
      (fun (h, m) v -> match String.trim v with
        | "" -> (h, m)
        | v' -> (h, KeyMap.update v' incr m))
      so_far
      (Csv.Row.to_list row)
    in
    let maps = Csv.Rows.fold_left ~f ~init chan' in
    let key_set_of_key_map m = KeySet.of_seq
      (Seq.map (fun (k, _) -> k) (KeyMap.to_seq m))
    in
    let likely_pk m = KeyMap.for_all (fun _ x -> x = 1) m in
    List.map (fun (h, m) -> (h, key_set_of_key_map m, likely_pk m)) maps
  in
  let chan = open_in_bin path in
  Fun.protect (fun () -> load_chan chan) ~finally:(fun () -> close_in chan)

let drop_empty tbl = List.filter (fun (_, m, _) -> not (KeySet.is_empty m)) tbl

let rec leave_one_out = function
  | [] -> []
  | hd::tl ->
      (hd, tl)::List.map (fun (el, rest) -> (el, hd::rest)) (leave_one_out tl)

let zip xs ys = List.map2 (fun x y -> (x, y)) xs ys

(* list/nondet monad *)
let (let*) xs f = List.concat_map f xs
let return x = [x]
let guard b xs = if b then xs else []

let possible_fks names tbls =
  let* ((fk_tbl, fk_cols), others) = leave_one_out (zip names tbls) in
  let* (col, keys, _) = fk_cols in
  let* (ref_tbl, ref_cols) = others in
  let* (col', keys', maybe_pk) = ref_cols in
  guard (col = col' && KeySet.subset keys keys' && maybe_pk)
    (return (fk_tbl, col, ref_tbl, col'))

let possible_fks_loose names tbls =
  let* ((fk_tbl, fk_cols), others) = leave_one_out (zip names tbls) in
  let* (col, keys, _) = fk_cols in
  let* (ref_tbl, ref_cols) = others in
  let* (col', keys', maybe_pk) = ref_cols in
  guard (KeySet.subset keys keys' && maybe_pk)
    (return (fk_tbl, col, ref_tbl, col'))

let () =
  let names = List.tl @@ Array.to_list Sys.argv in
  let tables = List.map (fun path -> path |> load |> drop_empty) names in
  let fks = possible_fks names tables in
  let loose_fks = possible_fks_loose names tables in
  List.iter (fun (fk_tbl, fk_col, ref_tbl, ref_col) ->
    Printf.printf "%s (%s) references %s (%s)\n" fk_tbl fk_col ref_tbl ref_col)
    fks;
  List.iter (fun (fk_tbl, fk_col, ref_tbl, ref_col) ->
    Printf.printf "MAYBE: %s (%s) references %s (%s)\n"
      fk_tbl fk_col ref_tbl ref_col)
    loose_fks
