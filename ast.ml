exception TypeError of string

type binop = Plus | Minus | Times | Divide | Mod | Eq | Neq | Lt | Leq | Gt | Geq | And | Or
            | Dotproduct | Mulmatrix | Angle | Power

type unop = | Not | Transpose | Mag | Dim | Abs | Determinant | Rows | Cols | Size

type expr = 
  | Int of int
  | Float of float
  | Bool of bool
  | Id of string
  | File of string
  | Vector of expr list
  | Matrix of expr list list
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | CreateVec of int * expr
  | CreateMat of int * int * expr
  | Rowswap of expr * int * int
  | Rowreduce of expr * int * float * int
  | Iszero of expr
  | Unit of int*int
  | GetVec of string*expr
  | Getmat of string*expr*expr
  | Input of string option
  | Inputvec of int * string option
  | InputMat of int * int * string option
  | Minor of string*expr*expr
  


type stmt = 
  | Assign of string * expr
  | AssignVec of string * expr * expr
  | AssignMat of string * expr * expr * expr
  | Print of expr
  | If_Then_Else of expr * stmt * stmt 
  | For of stmt * stmt * expr * stmt 
  | While of expr * stmt
  | Seq of stmt list
  | Block of stmt list
  | Raise of string
  
  type datatype = 
    Tint | Tfloat | Tbool | TFile | Tvector of int | Tmatrix of int * int 

  let datatype_to_string (dt : datatype) : string =
    match dt with
    | Tint -> "Tint"
    | Tfloat -> "Tfloat"
    | Tbool -> "Tbool"
    | TFile -> "TFile"
    | Tvector n -> "Tvector (" ^ string_of_int n ^ ")"
    | Tmatrix (i, j) -> "Tmatrix (" ^ string_of_int i ^ ", " ^ string_of_int j ^ ")"

  let variable_table : (string, datatype) Hashtbl.t = Hashtbl.create 100

  let add_variable (id: string) (dt: datatype) : unit =
    if Hashtbl.mem variable_table id then
      let existing_type = Hashtbl.find variable_table id in
      if existing_type <> dt then
        raise (TypeError ("Type mismatch: Variable '" ^ id ^ "' already declared with a different type"))
      else
        () (* If the type is the same, do nothing *)
    else
      Hashtbl.add variable_table id dt
  
  let tau (id: string) : datatype = 
    if Hashtbl.mem variable_table id then
      Hashtbl.find variable_table id
    else
      raise (TypeError "Variable not declared Earlier") 

  let rec to_string_expr (e : expr) : string =
    match e with
    | Int i -> "[Int (" ^ string_of_int i ^ ")]"
    | Float f -> "[Float (" ^ string_of_float f ^ ")]"
    | Bool b -> "[Bool (" ^ string_of_bool b ^ ")]"
    | Id s -> "[Id (" ^ s ^ ")]"
    | File s -> "[File (" ^ s ^ ")]"
    | Vector l -> "[Vector (" ^ "[" ^ (String.concat ", " (List.map to_string_expr l)) ^ "]" ^ ")]"
    | Matrix l ->
        "[Matrix (" ^ "[" ^ (String.concat ", " (
          List.map (fun row -> "[" ^ (String.concat ", " (List.map to_string_expr row)) ^ "]") l
        )) ^ "]" ^ ")]"
    | Binop (e1, op, e2) ->
        "[Binop (" ^ to_string_expr e1 ^ ", " ^
        (match op with
          | Plus -> "Plus" | Minus -> "Minus" | Times -> "Times" | Divide -> "Divide" | Mod -> "Mod"
          | Eq -> "Eq" | Neq -> "Neq" | Lt -> "Lt" | Leq -> "Leq" | Angle -> "Angle"
          | Gt -> "Gt" | Geq -> "Geq" | And -> "And" | Or -> "Or"
          | Dotproduct -> "Dotproduct" | Mulmatrix -> "Mulmatrix" | Power -> "Power") ^
        ", " ^ to_string_expr e2 ^ ")]"
    | Unop (op, e) ->
        "[Unop (" ^
        (match op with
          | Not -> "Not"
          | Transpose -> "Transpose"
          | Mag -> "Mag"
          | Dim -> "Dim"
          | Abs -> "Abs"
          | Determinant -> "Determinant"
          | Rows -> "Rows"
          | Cols -> "Cols"
          | Size -> "Size") ^ ", " ^ to_string_expr e ^ ")]"
    | CreateVec (i, e) ->
        "[CreateVec (" ^ string_of_int i ^ ", " ^ to_string_expr e ^ ")]"
    | CreateMat (i, j, e) ->
        "[CreateMat (" ^ string_of_int i ^ ", " ^ string_of_int j ^ ", " ^ to_string_expr e ^ ")]"
    | Rowswap (e, i, j) ->
        "[Rowswap (" ^ to_string_expr e ^ ", " ^ string_of_int i ^ ", " ^ string_of_int j ^ ")]"
    | Rowreduce (e, i, f, j) ->
        "[Rowreduce (" ^ to_string_expr e ^ ", " ^ string_of_int i ^ ", " ^ string_of_float f ^ ", " ^ string_of_int j ^ ")]"
    | Iszero e -> "[Iszero (" ^ to_string_expr e ^ ")]"
    | Unit (i, j) -> "[Unit (" ^ string_of_int i ^ ", " ^ string_of_int j ^ ")]"
    | GetVec (id, n) ->
        "[GetVec (" ^ id ^ ", " ^ to_string_expr n ^ ")]"
    | Getmat (id, n, m) ->
        "[Getmat (" ^ id ^ ", " ^ to_string_expr n ^ ", " ^ to_string_expr m ^ ")]"
    | Input f -> "[Input (" ^ (match f with None -> "None" | Some s -> "Some " ^ s) ^ ")]"
    | Inputvec (i, f) -> "[Inputvec (" ^ string_of_int i ^ ", " ^ (match f with None -> "None" | Some s -> "Some " ^ s) ^ ")]"
    | InputMat (i, j, f) -> "[InputMat (" ^ string_of_int i ^ ", " ^ string_of_int j ^ ", " ^ (match f with None -> "None " | Some s -> "Some " ^ s) ^ ")]"
    | Minor (id, i, j) ->
        "[Minor (" ^ id ^ ", " ^ to_string_expr i ^ ", " ^ to_string_expr j ^ ")]"

  let rec to_string_stmt (s : stmt) : string =
    match s with
    | Assign (s, e) -> "[Assign (" ^ s ^ ", " ^ to_string_expr e ^ ")]"
    | AssignVec (s, i, e) ->
      "[AssignVec (" ^ s ^ ", " ^ to_string_expr i ^ ", " ^ to_string_expr e ^ ")]"
    | AssignMat (s, i, j, e) ->
      "[AssignMat (" ^ s ^ ", " ^ to_string_expr i ^ ", " ^ to_string_expr j ^ ", " ^ to_string_expr e ^ ")]"
    | Print e -> "[Print (" ^ to_string_expr e ^ ")]"
    | If_Then_Else (e, s1, s2) ->
        "[If_Then_Else (" ^ to_string_expr e ^ ", " ^ to_string_stmt s1 ^ ", " ^ to_string_stmt s2 ^ ")]"
    | For (s1, s2, e, s3) ->
        "[For (" ^ to_string_stmt s1 ^ ", " ^ to_string_stmt s2 ^ ", " ^ to_string_expr e ^ ", " ^ to_string_stmt s3 ^ ")]"
    | While (e, s) ->
        "[While (" ^ to_string_expr e ^ ", " ^ to_string_stmt s ^ ")]"
    | Seq lst -> "[Seq (" ^ "[" ^ String.concat ", " (List.map to_string_stmt lst) ^ "]" ^ ")]"
    | Block lst -> "[Block (" ^ "[" ^ String.concat ", " (List.map to_string_stmt lst) ^ "]" ^ ")]"
    | Raise id -> "[Raise (" ^ id ^ ")]"
  ;;

