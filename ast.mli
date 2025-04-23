(* Type pour représenter les identifiants (noms de variables, fonctions, etc.) *)
type ident = string
type label = string

(* Enumération des types de données de base disponibles dans Rustine *)
type basic_type = 
  | I8 | I16 | I32 | I64 | I128 | ISize    (* Types d'entiers signés *)
  | U8 | U16 | U32 | U64 | U128 | USize    (* Types d'entiers non-signés *)
  | F32 | F64                              (* Types de nombres à virgule flottante *)
  | Bool                                   (* Type booléen *)
  | Unit                                   (* Type unit *)


  val ident_to_basic_type : string -> basic_type option




(* Types pour les expressions de type (y compris les références, les références mutables, etc.) *)
type type_expr =
  | Unit
  | UnknownType
  | BasicType of basic_type                (* Un type de base *)
  | Reference of type_expr                 (* Référence à un type *)
  | MutableReference of type_expr          (* Référence mutable à un type *)
  | ParenType of type_expr                 (* Type entouré de parenthèses *)

(* Types pour les constantes (entiers, flottants, booléens) *)
type const =
  | ConstInt of int                        (* Constante entière *)
  | ConstFloat of float                    (* Constante flottante *)
  | ConstBool of bool                      (* Constante booléenne *)

(* Opérateurs unaires (référence, déréférencement, moins, etc.) *)
type unary_op = 
  | Ref | MutRef | Deref | Neg | Not | Mut     (* Différents opérateurs unaires *)

(* Opérateurs binaires (addition, soustraction, multiplication, etc.) *)
type binary_op = 
  | Add | Sub | Mul | Div | Mod            (* Opérateurs arithmétiques *)
  | And | Or | Xor | Shl | Shr             (* Opérateurs bit à bit *)
  | Eq | Neq | Lt | Le | Gt | Ge           (* Opérateurs de comparaison *)
  | AndAnd | OrOr                          (* Opérateurs logiques *)

(* Types pour les expressions dans Rustine *)
type block_item = 
  | Decl of decla
  | Expr of expr

and block = block_item list 

and expr =
  | Const of const                         
  | Var of ident                           
  | UnaryOp of unary_op * expr             
  | BinaryOp of expr * binary_op * expr    
  | Cast of expr * type_expr               
  | Assign of ident * expr                 
  | If of expr * block * block option      
  | While of label option * expr * block
  | For of ident * expr * expr * expr      
  | Function of ident * (ident * type_expr) list * type_expr option * block
  | Block of block                         
  | FuncCall of expr * expr list           
  | Return of expr option                  
  | Loop of label option * block
  | Break of label option
  | Continue of label option
  | LabeledBlock of label option * block



and decla =
  | VarDecl of ident * type_expr option * expr option   
  | FuncDecl of ident * (ident * type_expr) list * type_expr option * block  


(* Type de programme *)
type programme = decla list

