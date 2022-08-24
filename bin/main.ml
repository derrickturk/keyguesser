module KeySet = Set.Make(String)

let load path =
  let load_chan chan =
    let chan' = Csv.of_channel ~has_header:true ~strip:false chan in
    let hdr = Csv.Rows.header chan' in
    let init = List.map (fun h -> (h, KeySet.empty)) hdr in
    let f so_far row = List.map2
      (fun (h, s) v -> match String.trim v with
        | "" -> (h, s)
        | v' -> (h, KeySet.add v' s))
      so_far
      (Csv.Row.to_list row)
    in
    Csv.Rows.fold_left ~f ~init chan'
  in
  let chan = open_in_bin path in
  Fun.protect (fun () -> load_chan chan) ~finally:(fun () -> close_in chan)

(*
let dump tbl =
  let dump_col (col, vals) =
    Printf.printf "%s: {%s}\n" col (String.concat ", " (KeySet.elements vals))
  in
  List.iter dump_col tbl
*)

let drop_empty tbl = List.filter (fun (_, s) -> not (KeySet.is_empty s)) tbl

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
  let* (col, keys) = fk_cols in
  let* (ref_tbl, ref_cols) = others in
  let* (col', keys') = ref_cols in
  guard (col = col' && KeySet.subset keys keys')
    (return (fk_tbl, col, ref_tbl, col'))

let possible_fks_loose names tbls =
  let* ((fk_tbl, fk_cols), others) = leave_one_out (zip names tbls) in
  let* (col, keys) = fk_cols in
  let* (ref_tbl, ref_cols) = others in
  let* (col', keys') = ref_cols in
  guard (KeySet.subset keys keys')
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
