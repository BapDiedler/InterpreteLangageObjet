open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let find_class cn =
    List.find (fun cdef -> idef.class_name (*voir kawa.ml*) = cn) p.classes 
  in
  and type_mem_access m tenv = match m with
    | Var x -> 
    | Field (objet, attribut) -> (*verifiet si type de objet est une class qui existe puis trouver les attribut de la classe et verifier si le type de attribut est le type d'un attribut de la classe de objet*)
      begin
        let typ_e = type_expr e tenv in
        match typ_e with
        | TClass cn ->
          let cdef = find_class cn in
          let _, attrib_type = List.find (fun (name, _) -> name = attribut) cdef.attributes in
          attrib_type
        | _ -> error "Accessing field of a non-class type"
      end
    | _ -> failwith "case not implemented in type_mem_access"
  in



  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | Var x -> Env.find x tenv
    | Unop (op, e) -> let _ = check e (match op with Opp -> TInt | Not -> TBool) tenv in TInt
    | Binop (op, e1, e2) -> begin
      match op with
      | Add | Sub | Mul | Div | Rem -> check e1 TInt tenv; check e2 TInt tenv; TInt
      | Lt | Le | Gt | Ge | Eq | Neq -> check e1 TInt tenv; check e2 TInt tenv; TBool
      | And | Or -> check e1 TBool tenv; check e2 TBool tenv; TBool
    end
    | Get (m) (*soit une variable (verifier qu'elle existe) soit un attribut (verifier que la classe existe et que l'attribut existe)*) -> type_mem_access m tenv
    | This ->  (*voir chap 8*) faiilwith "to do"
    | New (classe_n, args) -> (*trouver classe_n recuperer la methode constructeur et verifier si les argument sont les bon si c'est le cas on retourne la classe*)
                              begin
                              let cdef = find_class classe_n in
                              let constr = List.find (fun mdef -> mdef.method_name = "constructor") cdef.methods in
                              (*verifier que les arguments sont les bons*)
                              List.iter2 (fun a (_, t) -> check a t tenv) args constr.params;
                              TClass classe_n
                              end
    | MethCall (e, f, args) -> (*pareil mais on renvoie le type de retour de la methode*)begin
      let typ_e = type_expr e tenv in
      match typ_e with
        | TClass cn ->
          let cdef = find_class cn in
          let mdef = List.find (fun mdef -> mdef.method_name = f) cdef.methods in
          List.iter2 (fun a (_, t) -> check a t tenv) args mdef.params;
          mdef.return
        | _ -> error "Method call on non-class type"
        end
    | _ -> failwith "case not implemented in type_expr"

  

  (*if return set while ...*)
  let rec check_instr i ret tenv = match i with 
    | Print e -> check e TInt tenv
    | If(e, s1, s2) -> begin 
                        check e Tbool tenv;
                        check_seq s1 ret tenv;
                        check_seq s2 ret tenv
                      end
    |Set (m, e) -> begin 
      let tmp = type_expr m in
      check e tmp tenv
    | While (e, s) -> check e TBool tenv; check_seq s ret tenv
    | Return e -> check e ret tenv
    | Expr e -> type_expr e tenv
    | _ -> failwith "case not implemented in check_instr"
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in

  

  let rec check e tyo tenv = failwith "to do" 
  in

  let subclass t1 t2 = failwith "To Do" (*vérifier que t1 est une ss classe de t2*)
    let rec aux cn1 cn2 = failwith "to do"
    in
  in

  (*ajouter check class et check mdef *)


  check_seq p.main TVoid tenv
