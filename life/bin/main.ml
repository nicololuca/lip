module T = ANSITerminal
open Life.Main

let _ = match Array.length(Sys.argv) with
    3 -> let rule = Sys.argv.(1) in
    let k = int_of_string (Sys.argv.(2)) in
    T.erase T.Screen;
    T.save_cursor();
    Random.self_init();
    let parsed_rule = parse rule in
    let w = loop init_w k parsed_rule in
    display w;
    ignore(read_line());
    T.restore_cursor();
    print_newline()
  (* wrong usage *)
  | _ -> failwith "Usage: dune exec life n_rounds"
