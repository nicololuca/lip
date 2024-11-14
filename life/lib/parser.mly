%{
open Rule
%}

%token EXT
%token S_TOK
%token B_TOK
%token <string> SEQ
%token <string> RANGE_SEQ
%token SLASH
%token EOF

%start <rule> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | S_TOK; e1 = sequence; SLASH; B_TOK; e2 = sequence { Rule(e1,e2) }
  | EXT; S_TOK; e1 = ext_sequence; SLASH; B_TOK; e2 = ext_sequence { Rule(e1,e2) }
  | EXT; e1 = ext_sequence; SLASH; e2 = ext_sequence { Rule(e1,e2) }
;

sequence:
  | seq = SEQ { List.map (fun x -> (int_of_char x) - 48) (seq |> String.to_seq |> List.of_seq) }
;
ext_sequence:
  | seq = SEQ { (seq |> String.split_on_char ',' |> List.map (fun x -> int_of_string x)) }
  | seq = RANGE_SEQ { 
      seq 
      |> String.split_on_char ',' 
      |> List.map (fun x -> String.split_on_char '.' x)
      |> List.map (fun x -> List.filter (fun y -> not(y="")) x)
      |> List.map (fun x -> List.map (fun y -> int_of_string y) x)
      |> List.map (fun x -> match x with
          [] -> [0]
        | [a;b] -> List.init (b-a+1) (fun x -> a+x)
        | _ -> failwith "Wrong format"
      )
      |> List.flatten

    (*in
    let create_list_from_range (r: int list) = 
      let (a,b) = (List.nth r 1, List.nth r 2) in
      let l = List.init (b-a) (fun i -> a+i+1) in
      l
    in
    let rec create_lists i k =
      if i=k then [] 
      else create_list_from_range (List.nth ranges i)::create_lists (i+1) k
    in let l = create_lists 1 (List.length ranges) in
    l*)
  }
;