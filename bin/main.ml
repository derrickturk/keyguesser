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

let possible_fks names tbls =
  let candidates = leave_one_out (zip names tbls) in
  let pick_fks_for fk_tbl (col, keys) ref_tbl ref_cols =
    let is_match (col', keys') = col = col' && KeySet.subset keys keys' in
    let tag_match (col', _) = (fk_tbl, col, ref_tbl, col') in
    List.map tag_match (List.filter is_match ref_cols)
  in
  let pick_fks ((fk_tbl, fk_cols), tos) = List.concat_map
    (fun fk_col -> List.concat_map
      (fun (ref_tbl, ref_cols) -> pick_fks_for fk_tbl fk_col ref_tbl ref_cols)
      tos)
    fk_cols
  in
  List.concat_map pick_fks candidates

let possible_fks_loose names tbls =
  let candidates = leave_one_out (zip names tbls) in
  let pick_fks_for fk_tbl (col, keys) ref_tbl ref_cols =
    let is_match (_, keys') = KeySet.subset keys keys' in
    let tag_match (col', _) = (fk_tbl, col, ref_tbl, col') in
    List.map tag_match (List.filter is_match ref_cols)
  in
  let pick_fks ((fk_tbl, fk_cols), tos) = List.concat_map
    (fun fk_col -> List.concat_map
      (fun (ref_tbl, ref_cols) -> pick_fks_for fk_tbl fk_col ref_tbl ref_cols)
      tos)
    fk_cols
  in
  List.concat_map pick_fks candidates

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
