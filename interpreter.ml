open Ast

exception RuntimeError of string

(* Value types supported by the interpreter *)
type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VVector of value list         (* Vectors are homogeneous (ints or floats) *)
  | VMatrix of value list list    (* Matrices as list of rows *)
  | VUnit

(* Environment: mapping from variable names to their current value *)
module Env = struct
  type t = (string, value) Hashtbl.t
  let create () : t = Hashtbl.create 16
  let get env x =
    try Hashtbl.find env x
    with Not_found -> raise (RuntimeError ("Unbound variable: " ^ x))
  let set env x v = Hashtbl.replace env x v
end

(* At the beginning of the file, add file handle tracking *)
module FileHandles = struct
  type t = (string, in_channel) Hashtbl.t
  let handles : t = Hashtbl.create 16
  
  let get_or_create file =
    try Hashtbl.find handles file
    with Not_found -> 
      let ic = open_in file in
      Hashtbl.add handles file ic;
      ic
      
  let close_all () =
    Hashtbl.iter (fun _ ic -> close_in ic) handles;
    Hashtbl.clear handles
end

(* Helper: Convert a value to float for mixed arithmetic *)
let to_float = function
  | VInt n -> float_of_int n
  | VFloat f -> f
  | _ -> raise (RuntimeError "Expected a numeric value")

(* Helper: perform numeric binary operation that supports int/float *)
let numeric_binop op v1 v2 =
  match v1, v2 with
  | VInt n1, VInt n2 -> VInt (op n1 n2)
  | _ -> raise (RuntimeError "Invalid integer operation on non-integer types")

(* Evaluation of binary operations *)
let eval_binop op v1 v2 =
  match op, v1, v2 with
  (* Vector addition/subtraction *)
  | Plus, VVector l1, VVector l2 when List.length l1 = List.length l2 ->
      let added = List.map2 (fun a b ->
          match a, b with
          | VInt n1, VInt n2 -> VInt (n1 + n2)
          | _ -> VFloat (to_float a +. to_float b)
      ) l1 l2 in
      VVector added
  | Minus, VVector l1, VVector l2 when List.length l1 = List.length l2 ->
      let subbed = List.map2 (fun a b ->
          match a, b with
          | VInt n1, VInt n2 -> VInt (n1 - n2)
          | _ -> VFloat (to_float a -. to_float b)
      ) l1 l2 in
      VVector subbed
  (* Matrix addition/subtraction *)
  | Plus, VMatrix m1, VMatrix m2
      when List.length m1 = List.length m2 &&
           List.for_all2 (fun r1 r2 -> List.length r1 = List.length r2) m1 m2 ->
      let added = List.map2 (fun row1 row2 ->
          List.map2 (fun a b ->
              match a, b with
              | VInt n1, VInt n2 -> VInt (n1 + n2)
              | _ -> VFloat (to_float a +. to_float b)
          ) row1 row2
      ) m1 m2 in
      VMatrix added
  | Minus, VMatrix m1, VMatrix m2
      when List.length m1 = List.length m2 &&
           List.for_all2 (fun r1 r2 -> List.length r1 = List.length r2) m1 m2 ->
      let subbed = List.map2 (fun row1 row2 ->
          List.map2 (fun a b ->
              match a, b with
              | VInt n1, VInt n2 -> VInt (n1 - n2)
              | _ -> VFloat (to_float a -. to_float b)
          ) row1 row2
      ) m1 m2 in
      VMatrix subbed
  (* Numeric operations for ints and floats *)
  | Plus, (VInt _ | VFloat _), (VInt _ | VFloat _) ->
      (match v1, v2 with
       | VInt _, VInt _ -> numeric_binop ( + ) v1 v2
       | _ -> VFloat (to_float v1 +. to_float v2))
  | Minus, (VInt _ | VFloat _), (VInt _ | VFloat _) ->
      (match v1, v2 with
       | VInt _, VInt _ -> numeric_binop ( - ) v1 v2
       | _ -> VFloat (to_float v1 -. to_float v2))
  | Times, (VInt _ | VFloat _), (VInt _ | VFloat _) ->
      (match v1, v2 with
       | VInt _, VInt _ -> numeric_binop ( * ) v1 v2
       | _ -> VFloat (to_float v1 *. to_float v2))
  | Divide, (VInt _ | VFloat _), (VInt _ | VFloat _) ->
      let divisor = to_float v2 in
      if divisor = 0. then raise (RuntimeError "Division by zero")
      else (match v1, v2 with
            | VInt _, VInt _ -> VInt (int_of_float (to_float v1 /. divisor))
            | _ -> VFloat (to_float v1 /. divisor))
  | Mod, VInt n1, VInt n2 ->
      if n2 = 0 then raise (RuntimeError "Division by zero")
      else VInt (n1 mod n2)
  | Eq, _, _ -> VBool ( v1 =  v2)
  | Neq, _, _ -> VBool ( v1 <>  v2)
  | Lt, (VInt _ | VFloat _), (VInt _ | VFloat _) ->
      VBool (to_float v1 < to_float v2)
  | Leq, (VInt _ | VFloat _), (VInt _ | VFloat _) ->
      VBool (to_float v1 <= to_float v2)
  | Gt, (VInt _ | VFloat _), (VInt _ | VFloat _) ->
      VBool (to_float v1 > to_float v2)
  | Geq, (VInt _ | VFloat _), (VInt _ | VFloat _) ->
      VBool (to_float v1 >= to_float v2)
  | And, VBool b1, VBool b2 -> VBool (b1 && b2)
  | Or, VBool b1, VBool b2 -> VBool (b1 || b2)
  | Dotproduct, VVector l1, VVector l2 ->
      if List.length l1 <> List.length l2 then
        raise (RuntimeError "Vectors must be of the same dimension for dot product")
      else
        let dot = List.fold_left2 (fun acc a b -> acc +. (to_float a *. to_float b)) 0. l1 l2 in
        VFloat dot
  | Mulmatrix, VMatrix m1, VMatrix m2 ->
      let rows1 = List.length m1 in
      let cols1 = if rows1 = 0 then 0 else List.length (List.hd m1) in
      let rows2 = List.length m2 in
      let cols2 = if rows2 = 0 then 0 else List.length (List.hd m2) in
      if cols1 <> rows2 then raise (RuntimeError "Matrix dimensions do not match for multiplication")
      else
        let get_row m i = List.nth m i in
        let get_col m j = List.map (fun row -> List.nth row j) m in
        let mult_entry i j =
          let row = get_row m1 i in
          let col = get_col m2 j in
          List.fold_left2 (fun acc a b -> acc +. (to_float a *. to_float b)) 0. row col
        in
        let result = List.init rows1 (fun i ->
            List.init cols2 (fun j -> VFloat (mult_entry i j))
          )
        in VMatrix result
        | Mulmatrix, VVector v, VMatrix m ->
          let n = List.length v in
          let rows = List.length m in
          if rows <> n then
            raise (RuntimeError "Vector dimension and matrix rows do not match for multiplication")
          else
            let cols = if rows = 0 then 0 else List.length (List.hd m) in
            let get_col m j = List.map (fun row -> List.nth row j) m in
            let mult_entry j =
              List.fold_left2 (fun acc a b -> acc +. (to_float a *. to_float b))
                0. v (get_col m j)
            in
            let result = List.init cols (fun j -> VFloat (mult_entry j)) in
            VVector result
 | Mulmatrix, (VInt _ | VFloat _), VMatrix m ->
      let rows = List.length m in
      if rows = 0 then raise (RuntimeError "Matrix is empty")
      else
        let cols = List.length (List.hd m) in
        let result = List.init rows (fun i ->
            List.init cols (fun j -> VFloat ((to_float v1) *. (to_float (List.nth (List.nth m i) j)))))
        in VMatrix result
  | Power, (VInt _ | VFloat _), (VInt _ | VFloat _) ->
      let base = to_float v1 and exp = to_float v2 in
      VFloat (base ** exp)
  | _ -> raise (RuntimeError "Invalid binary operation or mismatched types")

(* Evaluation of unary operations *)
let eval_unop op v =
  match op, v with
  | Not, VBool b -> VBool (not b)
  | Abs, VInt n -> VInt (abs n)
  | Abs, VFloat f -> VFloat (abs_float f)
  | Transpose, VMatrix m ->
      let rows = List.length m in
      if rows = 0 then VMatrix [] else
      let cols = List.length (List.hd m) in
      let transposed = List.init cols (fun j -> List.init rows (fun i -> List.nth (List.nth m i) j)) in
      VMatrix transposed
  | Mag, VVector l ->
      let sumsq = List.fold_left (fun acc x -> acc +. (to_float x ** 2.)) 0. l in
      VFloat (sqrt sumsq)
  | Dim, VVector l -> VInt (List.length l)
  | Determinant, VMatrix m ->
      (* Check that matrix is square *)
      let n = List.length m in
      if n = 0 then raise (RuntimeError "Cannot compute determinant of an empty matrix")
      else if not (List.for_all (fun row -> List.length row = n) m) then
        raise (RuntimeError "Determinant is only defined for square matrices")
      else
        (* Helper: remove the element at index i from a list *)
        let remove_at i lst =
          let rec aux j = function
            | [] -> []
            | x :: xs -> if j = i then xs else x :: aux (j+1) xs
          in aux 0 lst
        in
        (* Helper: compute the minor matrix by removing row i and column j *)
        let minor_matrix m i j =
          List.map (fun row -> remove_at j row) (remove_at i m)
        in
        (* Recursive determinant computation using Laplace expansion *)
        let rec det m =
          match m with
          | [row] -> List.hd row
          | _ ->
              let first_row = List.hd m in
              let rec loop i acc lst =
                match lst with
                | [] -> acc
                | a :: rest ->
                    let sign = if i mod 2 = 0 then 1. else -1. in
                    let minor = minor_matrix m 0 i in
                    loop (i+1) (acc +. sign *. a *. det minor) rest
              in
              loop 0 0. first_row
        in
        (* Convert all entries to float and compute determinant *)
        VFloat (det (List.map (fun row -> List.map to_float row) m))
  | Rows, VMatrix m -> VInt (List.length m)
  | Cols, VMatrix m ->
      (match m with
       | [] -> VInt 0
       | row :: _ -> VInt (List.length row))
  | Size, VVector l -> VInt (List.length l)
  | _ -> raise (RuntimeError "Invalid unary operation or mismatched type")

(* Evaluate expressions recursively *)
let rec eval_expr (env: Env.t) (e: expr) : value =
  match e with
  | Int i -> VInt i
  | Float f -> VFloat f
  | Bool b -> VBool b
  | Id s -> Env.get env s
  | Vector lst ->
      let evaluated = List.map (eval_expr env) lst in
      VVector evaluated
  | Matrix rows ->
      let evaluated = List.map (fun row -> List.map (eval_expr env) row) rows in
      VMatrix evaluated
  | Binop (e1, op, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      eval_binop op v1 v2
  | Unop (op, e) ->
      let v = eval_expr env e in
      eval_unop op v
  | CreateVec (n, e) ->
      let v = eval_expr env e in
      let rec make_vec i =
        if i <= 0 then [] else v :: make_vec (i-1)
      in
      VVector (make_vec n)
  | CreateMat (n, m, e) ->
      let v = eval_expr env e in
      let rec make_row i =
        if i <= 0 then [] else v :: make_row (i-1)
      in
      let rec make_mat i =
        if i <= 0 then [] else (make_row m) :: make_mat (i-1)
      in
      VMatrix (make_mat n)
  | Rowswap (e, i, j) ->
      let v = eval_expr env e in
      (match v with
       | VMatrix m ->
           if i < 0 || j < 0 || i >= List.length m || j >= List.length m then
             raise (RuntimeError "Row index out of bounds")
           else
             let m' = Array.of_list m in
             let tmp = m'.(i) in
             m'.(i) <- m'.(j);
             m'.(j) <- tmp;
             VMatrix (Array.to_list m')
       | _ -> raise (RuntimeError "Rowswap applied to non-matrix"))
  | Rowreduce (e, i, f, j) ->
      let v = eval_expr env e in
      (match v with
       | VMatrix m ->
           if i < 0 || j < 0 || i >= List.length m || j >= List.length m then
             raise (RuntimeError "Row index out of bounds")
           else
             let m' = Array.of_list (List.map Array.of_list m) in
             let row_i = m'.(i) in
             let row_j = m'.(j) in
             if Array.length row_i <> Array.length row_j then
               raise (RuntimeError "Rows have different lengths")
             else begin
               for k = 0 to Array.length row_i - 1 do
                 row_j.(k) <- VFloat (to_float row_j.(k) +. f *. to_float row_i.(k))
               done;
               VMatrix (List.map Array.to_list (Array.to_list m'))
             end
       | _ -> raise (RuntimeError "Rowreduce applied to non-matrix"))
  | Iszero e ->
      let v = eval_expr env e in
      (match v with
       | VVector lst ->
           let is_zero x =
             match x with
             | VInt n -> n = 0
             | VFloat f -> f = 0.
             | _ -> false
           in VBool (List.for_all is_zero lst)
       | _ -> raise (RuntimeError "Iszero applied to non-vector"))
  | Unit (n, _) ->
      let rec make_unit i =
        if i = 0 then [] else (if i = n then VInt 1 else VInt 0) :: make_unit (i-1)
      in VVector (make_unit n)
  
      | GetVec (id, idx_expr) ->
        let v = Env.get env id in
        let idx = eval_expr env idx_expr in
        (match idx with
         | VInt i ->
             (match v with
              | VVector l ->
                  if i < 0 || i >= List.length l then
                    raise (RuntimeError "Vector index out of bounds")
                  else List.nth l i
              | VMatrix m ->
                  if i < 0 || i >= List.length m then
                    raise (RuntimeError "Matrix row index out of bounds")
                  else VVector (List.nth m i)
              | _ -> raise (RuntimeError "GetVec applied to non-vector/non-matrix"))
         | _ -> raise (RuntimeError "Vector index must be an integer"))
  | Getmat (id, i_expr, j_expr) ->
      let mat =
        match Env.get env id with
        | VMatrix m -> m
        | _ -> raise (RuntimeError "Getmat applied to non-matrix")
      in
      let i_val = eval_expr env i_expr in
      let j_val = eval_expr env j_expr in
      (match i_val, j_val with
       | VInt i, VInt j ->
           if i < 0 || i >= List.length mat then
             raise (RuntimeError "Matrix row index out of bounds")
           else
             let row = List.nth mat i in
             if j < 0 || j >= List.length row then
               raise (RuntimeError "Matrix column index out of bounds")
             else List.nth row j
       | _ -> raise (RuntimeError "Matrix indices must be integers"))
  | Inputvec (n, file_opt) ->
      let input_fn =
        match file_opt with
        | None -> read_line
        | Some file ->
            let ic = FileHandles.get_or_create file in
            fun () ->
              try input_line ic
              with End_of_file -> raise (RuntimeError "File ended prematurely")
      in
      let line = input_fn () in
      (* Parse the input as a vector in the format [1,2,3,4] *)
      let vector =
        try
          if String.length line < 2 || line.[0] <> '[' || line.[String.length line - 1] <> ']' then
            raise (RuntimeError "Invalid vector format");
          let elements = String.sub line 1 (String.length line - 2) |> String.split_on_char ',' in
          if List.length elements <> n then
            raise (RuntimeError "Vector dimension does not match the specified size");
          List.map (fun x -> VFloat (float_of_string (String.trim x))) elements
        with _ -> raise (RuntimeError "Invalid vector format or elements")
      in
      VVector vector

  | InputMat (n, m, file_opt) ->
      let input_fn =
        match file_opt with
        | None -> read_line
        | Some file ->
            let ic = FileHandles.get_or_create file in
            fun () ->
              try input_line ic
              with End_of_file -> raise (RuntimeError "File ended prematurely")
      in
      let line = input_fn () in
      (* Parse the input as a matrix in the format {[1,3,2],[1,3,2]} *)
      let matrix =
        try
          if String.length line < 3 || line.[0] <> '{' || line.[String.length line - 1] <> '}' then
            raise (RuntimeError "Invalid matrix format");
          
          (* Extract rows using bracket matching instead of simple splitting *)
          let extract_rows str =
            let len = String.length str in
            let rec parse pos acc current_row bracket_level =
              if pos >= len then List.rev acc
              else 
                let c = str.[pos] in
                match c with
                | '[' -> 
                    if bracket_level = 0 then
                      parse (pos + 1) acc (current_row ^ "[") (bracket_level + 1)
                    else
                      parse (pos + 1) acc (current_row ^ String.make 1 c) (bracket_level + 1)
                | ']' ->
                    if bracket_level = 1 then
                      let new_row = current_row ^ "]" in
                      parse (pos + 1) (new_row :: acc) "" 0
                    else
                      parse (pos + 1) acc (current_row ^ String.make 1 c) (bracket_level - 1)
                | ',' | ' ' | '\t' ->
                    if bracket_level = 0 then
                      (* Skip commas and whitespace between rows *)
                      parse (pos + 1) acc current_row bracket_level
                    else
                      parse (pos + 1) acc (current_row ^ String.make 1 c) bracket_level
                | _ ->
                    parse (pos + 1) acc (current_row ^ String.make 1 c) bracket_level
            in
            parse 1 [] "" 0  (* Start at pos 1 to skip the opening '{' *)
          in
          
          let row_strings = extract_rows line in
          if List.length row_strings <> n then
            raise (RuntimeError "Matrix row count does not match the specified size");
          
          List.map (fun row ->
            if String.length row < 2 || row.[0] <> '[' || row.[String.length row - 1] <> ']' then
              raise (RuntimeError "Invalid row format in matrix");
            let elements = String.sub row 1 (String.length row - 2) |> String.split_on_char ',' in
            if List.length elements <> m then
              raise (RuntimeError "Matrix column count does not match the specified size");
            List.map (fun x -> VFloat (float_of_string (String.trim x))) elements
          ) row_strings
        with e -> raise (RuntimeError ("Invalid matrix format or elements: " ^ Printexc.to_string e))
      in
      VMatrix matrix

  | Minor (id, i_expr, j_expr) ->
      let mat =
        match Env.get env id with
        | VMatrix m -> m
        | _ -> raise (RuntimeError "Minor applied to non-matrix")
      in
      let i_val = eval_expr env i_expr in
      let j_val = eval_expr env j_expr in
      (match i_val, j_val with
       | VInt i, VInt j ->
           if i < 0 || i >= List.length mat then
             raise (RuntimeError "Matrix row index out of bounds")
           else
             let mat' = List.mapi (fun row_idx row ->
                 if row_idx = i then [] else
                   List.filteri (fun col_idx _ -> col_idx <> j) row
               ) mat
             in VMatrix (List.filter (fun row -> row <> []) mat')
       | _ -> raise (RuntimeError "Matrix indices must be integers"))
  | Input file_opt ->
      let input_fn =
        match file_opt with
        | None -> read_line
        | Some file ->
            let ic = FileHandles.get_or_create file in
            fun () ->
              try input_line ic
              with End_of_file -> raise (RuntimeError "File ended prematurely")
      in
      let line = input_fn () in
      try VFloat (float_of_string line)
      with _ -> raise (RuntimeError "Invalid input: expected a floating-point number")

(* Evaluate statements; environment is updated in-place *)
let rec eval_stmt (env: Env.t) (s: stmt) : unit =
  match s with
  | Assign (id, e) ->
      let v = eval_expr env e in
      Env.set env id v
  | AssignVec (id, idx_expr, e) ->
      let v = Env.get env id in
      (match v with
       | VVector lst ->
           let idx = eval_expr env idx_expr in
           (match idx with
            | VInt i ->
                if i < 0 || i >= List.length lst then
                  raise (RuntimeError "Vector index out of bounds")
                else
                  let new_val = eval_expr env e in
                  let updated = List.mapi (fun j x -> if j = i then new_val else x) lst in
                  Env.set env id (VVector updated)
            | _ -> raise (RuntimeError "Vector index must be an integer"))
       | _ -> raise (RuntimeError "AssignVec applied to non-vector"))
  | AssignMat (id, i_expr, j_expr, e) ->
      let v = Env.get env id in
      (match v with
       | VMatrix m ->
           let i_val = eval_expr env i_expr and j_val = eval_expr env j_expr in
           (match i_val, j_val with
            | VInt i, VInt j ->
                if i < 0 || i >= List.length m then
                  raise (RuntimeError "Matrix row index out of bounds")
                else
                  let row = List.nth m i in
                  if j < 0 || j >= List.length row then
                    raise (RuntimeError "Matrix column index out of bounds")
                  else
                    let new_val = eval_expr env e in
                    let new_row = List.mapi (fun j' x -> if j' = j then new_val else x) row in
                    let new_mat = List.mapi (fun i' r -> if i' = i then new_row else r) m in
                    Env.set env id (VMatrix new_mat)
            | _ -> raise (RuntimeError "Matrix indices must be integers"))
       | _ -> raise (RuntimeError "AssignMat applied to non-matrix"))
  | Print e ->
      let v = eval_expr env e in
      let str =
        match v with
        | VInt n -> string_of_int n
        | VFloat f -> string_of_float f
        | VBool b -> string_of_bool b
        | VVector l ->
            let elems =
              List.map (fun x ->
                  match x with
                  | VInt n -> string_of_int n
                  | VFloat f -> string_of_float f
                  | VBool b -> string_of_bool b
                  | _ -> "..."
              ) l
            in
            "[" ^ String.concat ", " elems ^ "]"
        | VMatrix m ->
            let rows =
              List.map (fun row ->
                  let elems =
                    List.map (fun x ->
                        match x with
                        | VInt n -> string_of_int n
                        | VFloat f -> string_of_float f
                        | VBool b -> string_of_bool b
                        | _ -> "..."
                    ) row
                  in
                  "[" ^ String.concat ", " elems ^ "]"
              ) m
            in
            "{" ^ String.concat ",\n" rows ^ "}"
        | VUnit -> "unit"
      in
      print_endline str
  | If_Then_Else (cond, s1, s2) ->
      (match eval_expr env cond with
       | VBool true -> eval_stmt env s1
       | VBool false -> eval_stmt env s2
       | _ -> raise (RuntimeError "Condition must be a boolean"))
  | For (init, update, cond, body) ->
      eval_stmt env init;
      while (match eval_expr env cond with VBool b -> b | _ -> false) do
        eval_stmt env body;
        eval_stmt env update
      done
  | While (cond, body) ->
      while (match eval_expr env cond with VBool b -> b | _ -> false) do
        eval_stmt env body
      done
  | Seq lst | Block lst ->
      List.iter (eval_stmt env) lst
  | Raise id ->
      raise (RuntimeError ("Raised exception: " ^ id))

(* Top-level evaluation of a program (which is a statement) *)
let eval_program (p: stmt) : unit =
  let env = Env.create () in
  eval_stmt env p

