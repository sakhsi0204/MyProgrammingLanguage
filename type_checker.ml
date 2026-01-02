
open Ast 

exception Type_error of string

let rec type_chech_expr exp tau = match exp with
| Int i -> Tint
| Float f -> Tfloat
| Bool b -> Tbool
| Id s -> tau s
| File s -> TFile
| Vector l -> Tvector (List.length l)
| Matrix l -> Tmatrix (List.length l, List.length (List.hd l))
| Binop (e1, op, e2) -> 
    let tau1 = type_chech_expr e1 tau in
    let tau2 = type_chech_expr e2 tau in
    (match op with
    | Plus | Minus ->
        (match tau1,tau2 with
        | Tint, Tint -> Tint
        | Tfloat, Tint -> Tfloat
        | Tint, Tfloat -> Tfloat
        | Tfloat, Tfloat -> Tfloat
        | Tvector n, Tvector m when n = m -> Tvector n
        | Tmatrix (n, m), Tmatrix (n', m') when n = n' && m = m' -> Tmatrix (n, m)
        | _ -> raise (Type_error "Type Error in arithmetic operation"))
    |Times | Divide  -> 
        if tau1 = Tint && tau2 = Tint then Tint
        else if tau1 = Tfloat && tau2 = Tint then Tfloat
        else if tau1 = Tint && tau2 = Tfloat then Tfloat
        else if tau1 = Tfloat && tau2 = Tfloat then Tfloat
        else raise (Type_error "Type Error in arithmetic operation")
    | Mod -> 
        if tau1 = Tint && tau2 = Tint then Tint
        else raise (Type_error "Mod operation only works on integers")
    | Eq | Neq -> 
        if tau1 = tau2 then Tbool
        else if tau1 = Tint && tau2 = Tfloat then Tbool
        else if tau1 = Tfloat && tau2 = Tint then Tbool
        else raise (Type_error "Type Error in equality operation")
    | Lt | Leq | Gt | Geq -> 
        if tau1 = Tint && tau2 = Tint then Tbool
        else if tau1 = Tfloat && tau2 = Tint then Tbool
        else if tau1 = Tint && tau2 = Tfloat then Tbool
        else if tau1 = Tfloat && tau2 = Tfloat then Tbool
        else raise (Type_error "Type Error in comparison operation")
    | And | Or -> 
        if tau1 = Tbool && tau2 = Tbool then Tbool
        else raise (Type_error "Type Error in logical operation")
    | Dotproduct -> 
        (match tau1, tau2 with
        | Tvector n1, Tvector n2 when n1 = n2 -> Tfloat
        | Tvector _, Tvector _ -> raise (Type_error "Vectors of different dimensions")
        | _ -> raise (Type_error "Type Error in dot product operation"))
    | Mulmatrix -> 
        (match tau1, tau2 with
        | Tmatrix (n, m), Tmatrix (m2, p) when m = m2 -> Tmatrix (n, p)
        | Tmatrix (_, _), Tmatrix (_, _) -> raise (Type_error "Matrix dimensions do not match for multiplication")
        | Tfloat , Tmatrix (n, m) -> Tmatrix (n, m)
        | Tint , Tmatrix (n, m) -> Tmatrix (n, m)
        | Tvector n, Tmatrix (m, p) when n = m -> Tvector p
        | Tmatrix (n,m) , Tvector p when m = 1 -> Tmatrix (n,p)
        | _ -> raise (Type_error "Type Error in matrix multiplication operation"))
    | Angle -> 
        (match tau1, tau2 with
        | Tvector n1, Tvector n2 when n1 = n2 -> Tfloat
        | Tvector _, Tvector _ -> raise (Type_error "Vectors of different dimensions for angle calculation")
        | _ -> raise (Type_error "Type Error in angle operation"))
    
    | Power ->
        (match tau1, tau2 with
        | Tint, Tint -> Tfloat
        | Tfloat, Tint -> Tfloat
        | Tint, Tfloat -> Tfloat
        | Tfloat, Tfloat -> Tfloat
        | _ -> raise (Type_error "Type Error in power operation")
        )
    )
| Unop (op, e) ->
    let tau1 = type_chech_expr e tau in
    (match op with
    | Not -> 
        (match tau1 with
        | Tbool -> Tbool
        | _ -> raise (Type_error "Type Error in not operation"))
    | Transpose -> 
        (match tau1 with
        | Tmatrix (n, m) -> Tmatrix (m, n)
        | _ -> raise (Type_error "Type Error in transpose operation"))
    | Mag -> 
        (match tau1 with
        | Tvector _ -> Tfloat
        | _ -> raise (Type_error "Type Error in magnitude operation"))
    | Dim -> 
        (match tau1 with
        | Tvector _ -> Tint
        | _ -> raise (Type_error "Type Error in dimension operation"))
    | Abs -> 
        (match tau1 with
        | Tint -> Tint
        | Tfloat -> Tfloat
        | _ -> raise (Type_error "Type Error in absolute operation"))
    | Determinant -> 
        (match tau1 with
        | Tmatrix (n, n') when n = n' -> Tfloat
        | _ -> raise (Type_error "Type Error in determinant operation"))
    | Rows ->
        (match tau1 with
        | Tmatrix (_, _) -> Tint
        | _ -> raise (Type_error "Type Error in rows operation"))
    | Cols ->
        (match tau1 with
        | Tmatrix (_, _) -> Tint
        | _ -> raise (Type_error "Type Error in cols operation"))
    | Size ->
        (match tau1 with
        | Tvector n -> Tint
        | _ -> raise (Type_error "Type Error in size operation"))
    
        )
| CreateVec (i, e) ->
    if type_chech_expr e tau = Tint then Tvector i
    else if type_chech_expr e tau = Tfloat then Tvector i
    else raise (Type_error "Type Error in vector creation")
| CreateMat (i, j, e) ->
    if type_chech_expr e tau = Tint then Tmatrix (i, j)
    else if type_chech_expr e tau = Tfloat then Tmatrix (i, j)
    else raise (Type_error "Type Error in matrix creation")
| Rowswap (e, i, j) ->
    (match type_chech_expr e tau with
    | Tmatrix (n, m) -> Tmatrix (n, m)
    | _ -> raise (Type_error "Type Error in row swap"))
| Rowreduce (e, i, f, j) ->
    (match type_chech_expr e tau with
    | Tmatrix (n, m) -> Tmatrix (n, m)
    | _ -> raise (Type_error "Type Error in row reduce"))
| Iszero e ->
    (match type_chech_expr e tau with
    | Tvector _ -> Tbool
    | _ -> raise (Type_error "Type Error in iszero"))
| Unit (n, _) -> Tvector n
| GetVec (id, n) ->
    (match tau id with
    | Tvector size ->
        let tau_n = type_chech_expr n tau in
        (match tau_n with
        | Tint -> Tfloat    
        | _ -> raise (Type_error "Index must be of type int"))
    | Tmatrix (_,m) -> Tvector m
    | _ -> raise (Type_error "Indexing is only allowed on vectors"))

| Getmat (id, n, m) ->
    (match tau id with
    | Tmatrix (rows, cols) ->
        let tau_n = type_chech_expr n tau in
        let tau_m = type_chech_expr m tau in
        (match tau_n, tau_m with
        | Tint, Tint -> Tfloat
        | _ -> raise (Type_error "Indices must be of type int"))
    | _ -> raise (Type_error "Indexing is only allowed on matrices"))
  | Input _ -> Tfloat
  | Inputvec (n,_) -> Tvector n
    | InputMat (n, m, _) -> Tmatrix (n, m)

| Minor (id, n, m) ->
    (match tau id with
    | Tmatrix (rows, cols) ->
        let tau_n = type_chech_expr n tau in
        let tau_m = type_chech_expr m tau in
        (match tau_n, tau_m with
        | Tint, Tint -> Tmatrix (rows-1,cols-1)
        | _ -> raise (Type_error "Indices must be of type int"))
    | _ -> raise (Type_error "Indexing is only allowed on matrices"))
    
;;

let rec type_check_stmt stmt tau = match stmt with

| Assign (id, e) ->
    let tau1 = type_chech_expr e tau in
    add_variable id tau1
| AssignVec (id, n, e) ->
    (match tau id with
    | Tvector size ->
        let tau_n = type_chech_expr n tau in
        (match tau_n with
        | Tint ->
              let tau_e = type_chech_expr e tau in
              (match tau_e with
              | Tint | Tfloat -> ()
              | _ -> raise (Type_error "Assigned value must be of type int or float"))
        | _ -> raise (Type_error "Index must be of type int"))
    | _ -> raise (Type_error ("Variable '" ^ id ^ "' must be a vector")))
| Raise id -> ()
| AssignMat (id, n, m, e) ->
    (match tau id with
    | Tmatrix (rows, cols) ->
        let tau_n = type_chech_expr n tau in
        let tau_m = type_chech_expr m tau in
        (match tau_n, tau_m with
        | Tint, Tint ->
              let tau_e = type_chech_expr e tau in
              (match tau_e with
              | Tint | Tfloat -> ()
              | _ -> raise (Type_error "Assigned value must be of type int or float"))
        | _ -> raise (Type_error "Indices must be of type int"))
    | _ -> raise (Type_error ("Variable '" ^ id ^ "' must be a matrix")))
| Print e ->
    let _ = type_chech_expr e tau in ()
| If_Then_Else (e, s1, s2) ->
    let t1 = type_chech_expr e tau in 
    (match t1 with
    | Tbool -> type_check_stmt s1 tau; type_check_stmt s2 tau
    | _ -> raise (Type_error "Type Error in if statement"))
      
| For (s1, s2, e, s3) ->
  type_check_stmt s1 tau;
  let t2 = type_chech_expr e tau in
  (match t2 with
  | Tbool -> type_check_stmt s3 tau; type_check_stmt s2 tau
  | _ -> raise (Type_error "Type Error in for statement"))
| While (e, s) ->
   let t = type_chech_expr e tau in 
   (match t with
    | Tbool -> type_check_stmt s tau
    | _ -> raise (Type_error "Type Error in while statement"))
| Seq l -> List.iter (fun s -> type_check_stmt s tau) l
| Block l -> List.iter (fun s -> type_check_stmt s tau) l

;;