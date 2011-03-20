#load "Parser.fs"

//AST Types
type UnaryOp
  = Plus
  | Minus
  
type BinaryOp
  = Multiply
  | Add
  | Subtract
  | Divide
  | Assign
  | Equals

type Ast
  = Number of int
  | Identifier of string
  | String of string
  | Binary of BinaryOp * Ast * Ast
  | Unary of UnaryOp * Ast
  | Ternary of Ast * Ast * Ast // test * ifTrue * ifFalse
  | If of Ast * Ast * Ast option // test + ifTrue and possibly ifFalse (else branch)
  | Call of Ast * Ast list // target + arguments list
  | Block of Ast list // statements list
  | True
  | False

//Shorthand types for convenience
module P = Vaughan.Parser
type Token = string * string * (P.Pos)
type P = P.T<Token, Ast, string>

//Utility functions for extracting values out of Token
let type' ((t, _, _):Token) = t
let value ((_, v, _):Token) = v
let pos ((_, _, p):Token) = p
let value_num (t:Token) = t |> value |> int

//Utility functions for creating new tokens
let number value pos : Token = "#number", value, pos
let string value pos : Token = "#string", value, pos
let identifier name pos : Token = "#identifier", name, pos

let symbol type' pos : Token = type', "", pos
let add = symbol "+"
let sub = symbol "-"
let mul = symbol "*"
let div = symbol "/"
let assign = symbol "="
let equals = symbol "=="
let lparen = symbol "("
let rparen = symbol ")"
let lbrace = symbol "{"
let rbrace = symbol "}"
let comma = symbol ","
let qmark = symbol "?"
let colon = symbol ":"
let scolon = symbol ";"
let if' = symbol "if"
let true' = symbol "true"
let else' = symbol "else"

//Utility functions for converting tokens to binary and unary operators
let toBinaryOp tok =
  match type' tok with
  | "=" -> BinaryOp.Assign
  | "+" -> BinaryOp.Add
  | "-" -> BinaryOp.Subtract
  | "*" -> BinaryOp.Multiply
  | "/" -> BinaryOp.Divide
  | "==" -> BinaryOp.Equals
  | _ -> P.exn (sprintf "Couldn't convert %s-token to BinaryOp" (type' tok))

let toUnaryOp tok =
  match type' tok with
  | "+" -> UnaryOp.Plus
  | "-" -> UnaryOp.Minus
  | _ -> P.exn (sprintf "Couldn't convert %s-token to UnaryOp" (type' tok))

//Utility function for defining infix operators
let infix = 
  P.infix (fun token left right -> 
    Binary(token |> toBinaryOp, left, right))

//Utility function for defining right associative infix operators
let infixr symbol = 
  P.infixr (fun token left right -> 
    Binary(token |> toBinaryOp, left, right)) symbol
  
//Utility function for defining prefix operators
let prefix =
  P.prefix (fun token ast ->
    Unary(token |> toUnaryOp, ast))

//Utility function for parsing a block 
let block p =
  let rec stmts p =
    match p |> P.currentTypeTry with
    | None -> []
    | Some "}" -> p |> P.skip; []
    | _ -> (p |> P.stmt ";") :: (stmts p)

  p |> P.skipIf "{"
  Block(p |> stmts)

//Pretty printing function for error messages
let prettyPrint (tok:Token) =
  match tok with
  | "#number", value, _ -> sprintf "#number %s" value
  | "#identifier", name, _ -> sprintf "#identifier %s" name
  | "#string", value, _ -> sprintf "#string \"%s\"" value
  | type', _, _ -> type'

//The parser definition
let example_parser =
  (P.create type' pos (Some prettyPrint))

  //Literals and identifiers
  |> P.nud "#number" (fun t _ -> t |> value |> int |> Number) 
  |> P.nud "#identifier" (fun t _ -> t |> value |> Identifier)
  |> P.nud "#string" (fun t _ -> t |> value |> String)

  //Constants
  |> P.constant "true" Ast.True
  |> P.constant "false" Ast.False
  
  //Infix Operators <expr> <op> <expr>
  |> infix "==" 40
  |> infix "+" 50
  |> infix "-" 50
  |> infix "*" 60
  |> infix "/" 60
  |> infix "=" 80

  //Prefix Operators <op> <expr>
  |> prefix "+" 70
  |> prefix "-" 70
  
  //Grouping expressions (<expr>)
  |> P.nud "(" (fun t p -> p |> P.exprAndSkip ")")

  //Ternary operator <expr> ? <expr> : <expr>
  |> P.bpw "?" 70
  |> P.led "?" (fun _ test p ->
                                // test ?
      let ifTrue = p |> P.expr  // ifTrue
      p |> P.skipIf ":"         // :
      let ifFalse = p |> P.expr // ifFalse

      Ternary(test, ifTrue, ifFalse)
    )

  //If/Else statement if(<condition>) { <exprs } [else { <exprs> }]
  |> P.smd "if" (fun _ p ->
                              // if
      p |> P.skipIf "("       // (
      let test = p |> P.expr  // <test>
      p |> P.skipIf ")"       // )
      let ifTrue = p |> block // { block }

      match p |> P.skipIfTry "else" with
      | true -> If(test, ifTrue, Some(p |> block))
      | false -> If(test, ifTrue, None)
    )

  //Function call
  |> P.bpw "(" 80
  |> P.led "(" (fun _ func p ->
      let rec args (p:P) =
        match p |> P.currentType with
        | ")" -> p |> P.skip; []
        | "," -> p |> P.skip; args p
        | _ -> (p |> P.expr) :: args p

      Call(func, args p)
    )
    
//Code to parse
(*
1: x = 5;
2: y = 5;
3: 
4: if(x == y) {
5:   print("x equals y");
6: } else {
7:   print("x doesn't equal y");
8: }
*)

let code = @"x = 5;
y = 5;

if(x == y) {
  print('x equals y');
} else {
  print('x doesn't equal y');
}"

//The code in tokens, manually entered
//since we don't have a lexer to produce
//the tokens for us
let tokens = 
  [
    //x = 5;
    identifier "x" (1, 1)
    assign (1, 3)
    number "5" (1, 5)
    scolon (1, 6)

    //y = 5;
    identifier "y" (2, 1)
    assign (2, 3)
    number "5" (2, 5)
    scolon (2, 6)

    //if(x == y) {
    if' (4, 1)
    lparen (4, 3)
    identifier "x" (4, 4)
    equals (4, 6)
    identifier "y" (4, 9)
    rparen (4, 10)
    lbrace (4, 12)

    //print("x equals y");
    identifier "print" (5, 3)
    lparen (5, 8)
    string "x equals y" (5, 8)
    rparen (5, 21)
    scolon (5, 22)

    //} else {
    rbrace (6, 1)
    else' (6, 3)
    lbrace (6, 7)

    //print("x doesn't equal y");
    identifier "print" (7, 3)
    lparen (7, 7)
    string "x doesn't equal y" (7, 9)
    rparen (7, 27)
    scolon (7, 28)

    //}
    rbrace (8, 1)
  ]

let ast = example_parser |> P.runStmt tokens (Some code) ";"