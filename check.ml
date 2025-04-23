(* main.ml *)
open Ast
open Parser
open Lexer


(* Fonction pour lire un fichier et renvoyer son contenu sous forme de chaîne *)
let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

  let print_error lexbuf msg =
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.eprintf "Erreur %s à la ligne %d, position %d\n" msg pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
  
  let print_consts s =
    let lexbuf = Lexing.from_string s in
    try
      let ast = Parser.programme Lexer.token lexbuf in
      print_endline "Parsing réussi."
    with
    | Parsing.Parse_error ->
      print_error lexbuf "de syntaxe"
    | Lexer.Lexing_error msg ->
      print_error lexbuf ("lexicale: " ^ msg)
(* Type pour représenter les erreurs de correspondance de type *)
(* main.ml *)
open Ast
open Parser
open Lexer


(* Fonction pour lire un fichier et renvoyer son contenu sous forme de chaîne *)
let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

  let print_error lexbuf msg =
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.eprintf "Erreur %s à la ligne %d, position %d\n" msg pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
  
    let print_const = function
    | ConstInt(i) -> Printf.printf "%d\n" i  (* Pour les entiers *)
    | ConstFloat(f) -> Printf.printf "%g\n" f  (* Pour les flottants, %g ajuste le format *)
    | ConstBool(b) -> Printf.printf "%b\n" b  (* Pour les booléens *)
    (* Ajoutez d'autres cas si nécessaire *)
  
  (* Fonction récursive pour parcourir l'AST et imprimer les constantes *)
  let rec print_consts = function
    | Const(c) -> print_const c  (* Si c'est une constante, imprimez-la *)
    | UnaryOp(_, expr) | Cast(expr, _) -> print_consts expr
    | BinaryOp(expr1, _, expr2) -> print_consts expr1; print_consts expr2
    | If(expr, block1, block2_opt) -> 
        print_consts expr; 
        List.iter print_consts_block block1;
        (match block2_opt with | Some(block2) -> List.iter print_consts_block block2 | None -> ())
        | While(_, expr, block) ->
          print_consts expr; 
          List.iter print_consts_block block
      | Loop(_, block) ->
          List.iter print_consts_block block
    | Block(block) -> List.iter print_consts_block block
    | FuncCall(expr, expr_list) -> 
        print_consts expr; 
        List.iter print_consts expr_list
    | Return(opt_expr) -> (match opt_expr with | Some(expr) -> print_consts expr | None -> ())
    | Assign(_, expr) -> print_consts expr
    | Var(_) | LabeledBlock(_, _) | Continue(_) | Break(_) -> ()
  
  and print_consts_block = function
    | Decl(decla) -> print_consts_decl decla
    | Expr(expr) -> print_consts expr
  
  and print_consts_decl = function
    | VarDecl(_, _, Some(expr)) -> print_consts expr
    | FuncDecl(_, _, _, block) -> List.iter print_consts_block block
    | _ -> ()
  
  let print_consts_from_ast ast =
    match ast with
    | programme -> List.iter print_consts_decl programme


exception AffectError of string
      exception ScopeError of string
      
     (* (* Liste des identifiants pour les fonctions et les variables *)
      type scope = {
        functions: ident list;  (* Liste des noms de fonctions *)
        variables: ident list;  (* Liste des noms de variables *)
        mutable_references: ident list;  (* Liste des identifiants qui sont des références mutables *)
      }
      
      let rec check_scope_expr scope expr = match expr with
        | Const _ -> ()
        | Var id -> 
            if not (List.mem id scope.variables) then
              raise (ScopeError ("Variable non déclarée utilisée: " ^ id))
        | UnaryOp(_, e) -> check_scope_expr scope e
        | BinaryOp(e1, _, e2) -> 
            check_scope_expr scope e1;
            check_scope_expr scope e2
        | Cast(e, _) -> check_scope_expr scope e
        | Assign(id, e) -> 
            check_scope_expr scope e;
            if not (List.mem id scope.variables) then
              raise (ScopeError ("Assignation à une variable non déclarée: " ^ id))
        | If(cond, then_block, else_block) ->
            check_scope_expr scope cond;
            check_scope_block scope then_block;
            (match else_block with | Some b -> check_scope_block scope b | None -> ())
        | While(_, cond, block) -> 
            check_scope_expr scope cond;
            check_scope_block scope block
        | FuncCall(Var id, args) ->
            if not (List.mem id scope.functions) then
              raise (ScopeError ("Appel à une fonction non déclarée: " ^ id));
            List.iter (check_scope_expr scope) args
        | Block(b) -> check_scope_block scope b
        | _ -> () (* Traiter les autres cas *)
      
      and check_scope_block scope block =
        List.iter (fun item -> match item with
          | Expr e -> check_scope_expr scope e
          | Decl d -> check_scope_decl scope d
        ) block
      
      and check_scope_decl scope decl = match decl with
        | VarDecl(id, _, e_opt) ->
            let new_scope = { scope with variables = id :: scope.variables } in
            (match e_opt with | Some e -> check_scope_expr new_scope e | None -> ())
        | FuncDecl(id, _, _, body) ->
            if List.mem id scope.functions then
              raise (ScopeError ("Déclaration multiple de la fonction: " ^ id));
            let new_scope = { scope with functions = id :: scope.functions } in
            check_scope_block new_scope body
      
      let check_scope program =
        let initial_scope = { functions = []; variables = []; mutable_references = [] } in        List.iter (check_scope_decl initial_scope) program
  
        
        let rec check_affect_expr scope expr = match expr with
        | Assign( id, e) -> 
            (* Vérifiez si l'identifiant est une variable mutable ou une référence mutable. *)
            if not (List.mem id scope.mutable_references) then
              raise (AffectError ("Affectation non autorisée à une variable non mutable: " ^ id))
            else
              check_affect_expr scope e  (* Vérifiez aussi le côté droit de l'affectation *)
        | UnaryOp(Deref, Var id) -> 
            (* Vérifiez si l'identifiant est une référence mutable *)
            if not (List.mem id scope.mutable_references) then
              raise (AffectError ("Déréférencement non autorisé d'une référence non mutable: " ^ id))
        | Block(b) -> check_affect_block scope b
        | _ -> ()  (* Continuez pour les autres types d'expressions *)
      
      and check_affect_block scope block =
        List.iter (fun item -> match item with
          | Expr e -> check_scope_expr scope e
          | Decl d -> check_scope_decl scope d
        ) block
      

        let rec check_affect_decl scope decl = match decl with
          | VarDecl(id, Some MutableReference _, _) ->
              { scope with mutable_references = id :: scope.mutable_references }
          | VarDecl(id, _, _) ->
              if List.mem id scope.variables then
                raise (AffectError ("Tentative de réaffectation non mutable: " ^ id))
              else
                scope
          | FuncDecl(id, _, _, _) ->
              if List.mem id scope.functions then
                { scope with functions = id :: scope.functions }
              else
                raise (AffectError ("Fonction non déclarée ou non mutable: " ^ id))
          | _ -> scope  (* Pour les autres cas, retourner le scope inchangé *)
        
      
          let check_affect program =
            let initial_scope = { functions = []; variables = []; mutable_references = [] } in
            try
              let final_scope = List.fold_left check_affect_decl initial_scope program in
              (* Utiliser final_scope si nécessaire *)
              print_endline "Aucune erreur d'affectation détectée."
            with
            | AffectError msg -> Printf.eprintf "Erreur d'affectation : %s\n" msg
          
*)



let () =
let filename = Sys.argv.(1) in
let input = read_file filename in
let lexbuf = Lexing.from_string input in
try
  let ast = Parser.programme Lexer.token lexbuf in
  print_endline "Parsing réussi.";
  (* Après avoir réussi le parsing, imprimez toutes les constantes *)
  print_endline "Constantes trouvées :";
  print_consts_from_ast ast;  (* Assurez-vous que la fonction print_consts_from_ast est définie *)
  print_endline "Fin des constantes."
with
| Parsing.Parse_error ->
  print_error lexbuf "de syntaxe"
| Lexer.Lexing_error msg ->
  print_error lexbuf ("lexicale: " ^ msg)
| AffectError msg ->  (* Assurez-vous que l'exception AffectError est bien définie et capturée *)
  Printf.eprintf "Erreur d'affectation : %s\n" msg
