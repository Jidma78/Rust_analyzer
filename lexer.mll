{
  open Parser (* Pour utiliser les tokens définis dans le parser *)

  (* Convertit une chaîne en un entier *)
  let convertir_en_entier s = int_of_string s
      
  (* Convertit une chaîne en un flottant *)
  let convertir_en_flottant s = float_of_string s

  (* Exception pour les erreurs de lexing *)
  exception Lexing_error of string

  (* Retire les underscores et les suffixes des littéraux numériques *)
  let nettoyer_literal s =
    let sans_underscores = String.concat "" (String.split_on_char '_' s) in
    let suffixes = ["i8"; "i16"; "i32"; "i64"; "i128"; "isize"; "u8"; "u16"; "u32"; "u64"; "u128"; "usize"; "f32"; "f64"] in
    let rec remove_suffix str = function
      | [] -> str
      | suffix :: rest ->
          if String.ends_with ~suffix str then
            String.sub str 0 (String.length str - String.length suffix)
          else
            remove_suffix str rest
    in
    remove_suffix sans_underscores suffixes

  (* Variable pour suivre les niveaux de commentaires imbriqués *)
  let nest = ref 0   
}

(* Règles pour les identifiants *)
let identifiant = '_'? ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let identifiant_special = "r#" ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* Règles pour les mots-clés *)
let mot_clef = "as" | "break" | "continue" | "else" | "false" | "fn" | "if" | "let" | "loop" | "mut" | "return" | "true" | "while" 
let mot_clef_reserve =  "abstract" | "async" | "await" | "become" | "box" | "const" | "crate" | "do" | "dyn" | "enum" | "extern" | "final" | "for" | "impl" | "in" | "macro" | "match" | "mod" | "move" | "override" | "priv" | "pub" | "ref" | "self" | "Self" | "static" | "struct" | "super" | "trait" | "type" | "typeof" | "unsafe" | "unsized" | "use" | "virtual" | "where" | "yield"

(* Règles pour les constantes numériques *)
let chiffre = ['0'-'9']
let underscore = '_'
let chiffres_underscores = chiffre | underscore
let base_decimal = chiffre chiffres_underscores*
let base_binaire = "0b" ['0'-'1' '_']+
let base_octale = "0o" ['0'-'7' '_']+
let base_hexa = "0x" ['0'-'9' 'a'-'f' 'A'-'F' '_']+


let suffixe_type_entier = "i8" | "i16" | "i32" | "i64" | "i128" | "isize" | "u8" | "u16" | "u32" | "u64" | "u128" | "usize"
let entier = base_decimal suffixe_type_entier? | base_binaire suffixe_type_entier? | base_octale suffixe_type_entier? | base_hexa suffixe_type_entier? | base_decimal "_" suffixe_type_entier

let exposant = ['e' 'E'] ['+' '-']? ['0'-'9' '_']+
let suffixe_type_flottant = "f32" | "f64"
let flottant =  base_decimal suffixe_type_flottant | base_decimal "." base_decimal suffixe_type_flottant? | base_decimal "." base_decimal exposant suffixe_type_flottant? | base_decimal "." | base_decimal exposant suffixe_type_flottant?
let type_base = "i8" | "i16" | "i32" | "i64" | "i128" | "isize" | "u8" | "u16" | "u32" | "u64" | "u128" | "usize" | "f32" | "f64" | "bool" 

(* Règles pour les labels que je n'ai pas réussi à implémenter *)
let label = '\'' ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* Règles pour les opérateurs unaires et binaires *)
let operateur_unaire = "&" | "&mut" | "-" | "!" | "*"
let operateur_binaire = "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" | "=" | "!=" | "<" | "<=" | ">" | ">=" | "&&" | "||"

(* Règles pour les symboles de structure de contrôle *)
let structure_controle = "{" | "}" | "(" | ")" | ";" | "," | "->"
let spaces = [' ' '\t' '\n' '\r']+
let label_with_colon = label ':' 

(* Règles principales pour les tokens *)
rule token = parse
  | spaces { token lexbuf }  (* Ignorer les espaces blancs *)
  | "//" { line_comment lexbuf }  (* Commentaire de ligne *)
  | "/*" { nest := 1; comment lexbuf }  (* Commentaire multiligne *)

  (* Matching des différents tokens *)
  | label_with_colon as lbl_colon { LABEL(lbl_colon) }
  | label as lbl { LABEL(lbl) }
  | "&" spaces? "mut" { MUTREF }
  | "mut" { MUT }
  | "==" { EQ }
  | ':' { COLON }
  | "_" { IDENT("_") }
  | ";" { SEMICOLON }


  (* Matching des mots-clés réservés *)
  | mot_clef_reserve as kw_reserve {   
    match kw_reserve with
    | "abstract" -> ABSTRACT
    | "async" -> ASYNC
    | "await" -> AWAIT
    | "become" -> BECOME
    | "box" -> BOX
    | "const" -> CONST
    | "crate" -> CRATE
    | "do" -> DO
    | "dyn" -> DYN
    | "enum" -> ENUM
    | "extern" -> EXTERN
    | "final" -> FINAL
    | "for" -> FOR
    | "impl" -> IMPL
    | "in" -> IN
    | "macro" -> MACRO
    | "match" -> MATCH
    | "mod" -> MOD
    | "move" -> MOVE
    | "override" -> OVERRIDE
    | "priv" -> PRIV
    | "pub" -> PUB
    | "ref" -> REF
    | "self" -> SELF
    | "Self" -> SELFCAP
    | "static" -> STATIC
    | "struct" -> STRUCT
    | "super" -> SUPER
    | "trait" -> TRAIT
    | "typeof" -> TYPEOF
    | "unsafe" -> UNSAFE
    | "unsized" -> UNSIZED
    | "use" -> USE
    | "virtual" -> VIRTUAL
    | "where" -> WHERE
    | "yield" -> YIELD
    | _ -> raise (Lexing_error ("Mot réservé non reconnu: " ^ kw_reserve))
}


  (* Matching des autres mots-clés *)
  | mot_clef as kw {match kw with
        | "as" -> AS
        | "break" -> BREAK
        | "continue" -> CONTINUE
        | "else" -> ELSE
        | "false" -> FALSE
        | "if" -> IF
        | "let" -> LET
        | "loop" -> LOOP
        | "mut" -> MUT
        | "return" -> RETURN
        | "true" -> TRUE
        | "while" -> WHILE
        | "fn" -> FN
        | _ -> raise (Lexing_error ("Mot-clé non reconnu: " ^ kw))}

  (* Matching des types de base *)
  | type_base as tp { TYPE(tp) }

  (* Matching des identifiants *)
  | identifiant as id { IDENT(id) }
  | identifiant_special as id { IDENT(String.sub id 2 (String.length id - 2)) } 

  (* Matching des littéraux numériques *)
  | flottant as fnum { 
        let cleaned = nettoyer_literal fnum in
        print_endline ("Float: " ^ cleaned);  
        FLOAT(convertir_en_flottant cleaned) }
  | entier as num {  let cleaned = nettoyer_literal num in
        INT(convertir_en_entier cleaned)  }

  (* Matching des opérateurs *)
  | operateur_unaire as op {match op with
        | "&mut" -> MUTREF
        | "!" -> NOT
        | "mut" -> MUT 
        | "&" -> AND
        | "-" -> MINUS
        | "*" -> TIMES

        | _ -> raise (Lexing_error ("Opérateur unaire non reconnu: " ^ op))}
  | operateur_binaire as op {  match op with
        | "+" -> PLUS
        | "-" -> MINUS
        | "/" -> DIV
        | "*" -> TIMES
        | "%" -> MOD
        | "|" -> OR
        | "&" -> AND
        | "^" -> XOR
        | "<<" -> SHL
        | ">>" -> SHR
        | "==" -> EQ
        | "=" -> ASSIGN
        | "!=" -> NEQ
        | "<" -> LT
        | "<=" -> LE
        | ">" -> GT
        | ">=" -> GE
        | "&&" -> ANDAND
        | "||" -> OROR
        | _ -> raise (Lexing_error ("Opérateur binaire non reconnu: " ^ op))}

  (* Matching des symboles de structure de contrôle *)
  | structure_controle as sc { match sc with
        | ":" ->  COLON 
        | "{" -> LBRACE
        | "}" -> RBRACE
        | "(" -> LPAREN
        | ")" -> RPAREN
        | ";" -> SEMICOLON
        | "," -> COMMA
        | "->" -> ARROW
        | _ -> raise (Lexing_error ("Symbole de structure de contrôle non reconnu: " ^ sc)) }

  | eof { EOF }  (* Fin de fichier *)
  | _ { raise (Lexing_error "Erreur lexicale") } 

(* Règles pour les commentaires *)
and line_comment = parse
  | '\n' { token lexbuf }  (* Fin du commentaire de ligne *)
  | eof  { EOF }
  | _    { line_comment lexbuf }  (* Ignore le reste de la ligne *)

and comment = parse
  | "*/" { nest := !nest - 1; if !nest = 0 then token lexbuf else comment lexbuf }  (* Ferme un commentaire imbriqué *)
  | "/*" { nest := !nest + 1; comment lexbuf }  (* Ouvre un nouveau niveau de commentaire imbriqué *)
  | eof  { raise (Lexing_error "Commentaire non fermé") }
  | _    { comment lexbuf }  (* Continue à lire le commentaire *)
