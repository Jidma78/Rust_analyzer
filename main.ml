(* main.ml *)

(* Ouverture des modules nécessaires *)
open Ast
open Parser
open Lexer

(* Définition des exceptions personnalisées pour les erreurs spécifiques *)
exception AffectError of string
exception ScopeError of string


(* Lecture d'un fichier et renvoi de son contenu sous forme de chaîne *)
let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
                                          (* ---------- Question 3 ---------- *)

(* Affichage formaté des erreurs lexicales et syntaxiques *)
let print_error lexbuf msg =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.eprintf "Erreur %s à la ligne %d, position %d\n" msg pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

(* Affichage des constantes trouvées dans l'AST *)
let print_const = function
  | ConstInt(i) -> Printf.printf "%d\n" i
  | ConstFloat(f) -> Printf.printf "%g\n" f
  | ConstBool(b) -> Printf.printf "%b\n" b
  (* Ajouter d'autres types de constantes si nécessaire *)

(* Parcours récursif de l'AST pour imprimer toutes les constantes trouvées *)
(* Cette fonction récursive imprime toutes les constantes trouvées dans l'AST. *)
let rec print_consts = function
  | Const(c) -> print_const c  (* Appelle print_const pour imprimer la constante directement. *)
  | UnaryOp(_, expr) | Cast(expr, _) -> print_consts expr (* Pour les opérations unaires et les casts, continue de parcourir récursivement l'expression. *)
  | BinaryOp(expr1, _, expr2) -> 
      print_consts expr1; (* Traite récursivement l'expression à gauche de l'opérateur. *)
      print_consts expr2 (* Puis traite récursivement l'expression à droite. *)
  | If(expr, block1, block2_opt) -> 
      print_consts expr; (* Traite la condition du if. *)
      List.iter print_consts_block block1; (* Traite toutes les instructions dans le premier bloc du if. *)
      (match block2_opt with
      | Some(block2) -> List.iter print_consts_block block2 (* Traite le bloc else s'il existe. *)
      | None -> ())
  | While(_, expr, block) ->
      print_consts expr; (* Traite la condition du while. *)
      List.iter print_consts_block block (* Traite toutes les instructions dans le bloc du while. *)
  | Loop(_, block) ->
      List.iter print_consts_block block (* Traite toutes les instructions dans le bloc du loop. *)
  | Block(block) -> List.iter print_consts_block block (* Traite toutes les instructions dans un bloc générique. *)
  | FuncCall(expr, expr_list) -> 
      print_consts expr; (* Traite l'expression de l'appel de fonction. *)
      List.iter print_consts expr_list (* Traite toutes les expressions passées en arguments de la fonction. *)
  | Return(opt_expr) -> 
      (match opt_expr with 
      | Some(expr) -> print_consts expr (* Traite l'expression retournée par le return, si elle existe. *)
      | None -> ())
  | Assign(_, expr) -> print_consts expr (* Traite l'expression dans une assignation. *)
  | Var(_) | LabeledBlock(_, _) | Continue(_) | Break(_) | (For (_, _, _, _)|Function (_, _, _, _))-> () (* Ignore ces constructions pour l'impression des constantes. *)
and print_consts_block = function
  | Decl(decla) -> print_consts_decl decla (* Traite une déclaration dans un bloc. *)
  | Expr(expr) -> print_consts expr (* Traite une expression dans un bloc. *)

and print_consts_decl = function
  | VarDecl(_, _, Some(expr)) -> print_consts expr (* Traite l'initialisation d'une variable, si elle est présente. *)
  | FuncDecl(_, _, _, block) -> List.iter print_consts_block block (* Traite toutes les instructions dans le corps d'une fonction. *)
  | _ -> () (* Ignore les autres déclarations. *)

(* Entrée principale pour imprimer les constantes à partir de l'AST donné. *)
let print_consts_from_ast ast =
  match ast with
  | programme -> List.iter print_consts_decl programme

                                              (* ---------- Vérification de la Portée (Question 4) ---------- *)

(* Type pour représenter l'environnement de portée *)
type scope = {
  mutable vars: ident list;  (* Liste des variables déclarées *)
  mutable funcs: ident list  (* Liste des fonctions déclarées *)
}

(* Environnement initial sans variables ni fonctions *)
let initial_env = {vars = []; funcs = []}

(* Vérification de la portée dans un bloc d'instructions *)
let rec check_scope_block env = function
  | [] -> ()
  | Decl(decla) :: rest -> check_scope_decl env decla; check_scope_block env rest
  | Expr(expr) :: rest -> check_scope_expr env expr; check_scope_block env rest

(* Vérification de la portée dans une expression *)
and check_scope_expr env = function
      | Var(ident) ->
      if not (List.exists ((=) ident) env.vars) then
        raise (ScopeError ("Variable non déclarée: " ^ ident))
        
      | FuncCall(f_expr, args) ->
          check_scope_expr env f_expr;  (* vérifier la portée de l'expression de la fonction *)
          List.iter (check_scope_expr env) args
      | UnaryOp(_, expr) | Cast(expr, _) -> check_scope_expr env expr
      | BinaryOp(expr1, _, expr2) -> check_scope_expr env expr1; check_scope_expr env expr2
      | If(expr, block1, block2_opt) -> 
      check_scope_expr env expr;
      check_scope_block env block1;
      (match block2_opt with | Some(block2) -> check_scope_block env block2 | None -> ())
      | While(_, expr, block) ->
        print_consts expr;
        List.iter print_consts_block block
      | Loop(_, block) ->
        List.iter print_consts_block block
    
      | Block(block) ->
          let new_env_for_block = {env with vars = []} in  (* Nouvelle portée pour le bloc *)
          check_scope_block new_env_for_block block
      
      
      | Assign(ident, expr) ->
      if not (List.exists ((=) ident) env.vars) then
        raise (ScopeError ("Affectation à une variable non déclarée: " ^ ident));
      check_scope_expr env expr
      | _ -> ()  (* Ajoutez d'autres cas pour d'autres types d'expressions *)





(* Vérification de la portée dans une déclaration (variable ou fonction) *)

and check_scope_decl env = function
    | VarDecl(ident, _, _) ->
    env.vars <- ident :: List.filter ((<>) ident) env.vars  (* Masquage de variables autorisé *)
    | FuncDecl(ident, params, _, block) ->
      if List.exists ((=) ident) env.funcs then
        raise (ScopeError ("Fonction déjà déclarée: " ^ ident))
       else begin
        env.funcs <- ident :: env.funcs;  (* Ajoute la fonction à l'environnement *)
       let env_with_params = {env with vars = (List.map fst params) @ env.vars} in  (* Ajoute les paramètres à la portée des variables *)
        check_scope_block env_with_params block  (* Vérifie la portée à l'intérieur du corps de la fonction *)
    end


(* Lancement de la vérification de la portée à partir du nœud racine de l'AST *)
let check_scope_program (program : programme) =
  List.iter (check_scope_decl initial_env) program

(* ---------- Point d'Entrée Principal ---------- *)

let () =
  let filename = Sys.argv.(1) in
  let input = read_file filename in
  let lexbuf = Lexing.from_string input in
  try
    (* Parsing du programme et récupération de l'AST *)
    let ast = Parser.programme Lexer.token lexbuf in
    print_endline "Parsing réussi.";

    (* Vérification de la portée *)
    print_endline "Vérification de la portée...";
    check_scope_program ast;  (* Utilisation de la fonction de vérification *)
    print_endline "Aucune erreur de portée détectée.";

    (* Affichage des constantes trouvées *)
    print_endline "Constantes trouvées :";
    print_consts_from_ast ast;  
    print_endline "Fin des constantes."
  with
    | Parsing.Parse_error -> print_error lexbuf "de syntaxe"
    | Lexer.Lexing_error msg -> print_error lexbuf ("lexicale: " ^ msg)
    | AffectError msg -> Printf.eprintf "Erreur d'affectation : %s\n" msg
    | ScopeError msg -> Printf.eprintf "Erreur de portée : %s\n" msg
