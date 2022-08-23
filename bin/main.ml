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

let dump tbl =
  let dump_col (col, vals) =
    Printf.printf "%s: {%s}\n" col (String.concat ", " (KeySet.elements vals))
  in
  List.iter dump_col tbl

let () =
  let args = List.tl @@ Array.to_list Sys.argv in
  let tables = List.map load args in
  List.iter2 (fun path tbl -> print_endline path; dump tbl; print_endline "")
    args
    tables
