namespace Vaughan

(*
  F# implementation of a generic Top-Down-Operator-Precedence Parser 
  as described in this paper http://portal.acm.org/citation.cfm?id=512931.
*)

module Parser = 

  type Pos = int * int

  type T<'a, 'b, 'c> when 'c : comparison = {
      Input : 'a list ref
      Source : string option

      Type : 'a -> 'c
      Position : 'a -> Pos
      PrettyPrint : ('a -> string) option

      BindingPower : Map<'c, int>
      Null : Map<'c, 'a -> T<'a, 'b, 'c> -> 'b>
      Stmt : Map<'c, 'a -> T<'a, 'b, 'c> -> 'b>
      Left : Map<'c, 'a -> 'b -> T<'a, 'b, 'c> -> 'b>
  }

  type Exn (msg, pos) = 
    inherit System.Exception(msg)
    member x.Position = pos

  (*
    Creates a string error snippet 
    that points out the exact source position
    where the error occured, for example:
  
    4: if(x == y) {
    5:   print'x equals y');
    ----------^
  *)
  let private errorSource pos source =
    
    let splitLines (text:string) = 
      let text = text.Replace("\r\n", "\n").Replace("\r", "\n")
      System.Text.RegularExpressions.Regex.Split(text, "\n")

    let lineNum (input:int) n = 
      (input.ToString()).PadLeft(n, '0')

    let stringRepeat n input =
      if System.String.IsNullOrEmpty input then input
      else
        let result = new System.Text.StringBuilder(input.Length * n)
        result.Insert(0, input, n).ToString()

    match source with
    | None -> ""
    | Some(source:string) -> 
      let source = source |> splitLines 
      let result = ref ""
      let line, column = pos

      if line <= source.Length && line > 1 then
        let nr = line.ToString()
        let nrl = nr.Length

        //previous line
        let pline = line - 1
        if pline >= 1 then 
          let num = lineNum pline nrl
          result := num+": "+source.[pline-1]+"\n"

        //current line
        let text = source.[line-1]
        if column <= text.Length then
          let arrow = "-" |> stringRepeat (nrl + column + 1)
          result := !result+nr+": "+text+"\n"+arrow+"^\n"

      !result

  (*Pretty prints a token*)
  let private prettyPrint parser token =
    match parser.PrettyPrint with
    | None -> (token |> parser.Type).ToString()
    | Some f -> token |> f

  (*A simple exception, no source position*)
  let exn msg = Exn(msg, (0, 0)) |> raise

  (*An exception with a source position*)
  let exnLine pos msg = 
    let line = sprintf "Error on line: %i col: %i\n" (fst pos) (snd pos)
    Exn(line + msg, pos) |> raise

  (*An exception with source snippet and source position*)
  let exnSource token parser message =
    let pos = token |> parser.Position
    let source = parser.Source |> errorSource pos 
    (source + message) |> exnLine pos

  let private unexpectedEnd () = "Unexpected end of input" |> exn
  let private unexpectedToken token parser =
    let type' = token |> prettyPrint parser
    let unexpected = sprintf "Unexpected: %s"  type'
    exnSource token parser unexpected

  (*Returns the binding power for @token*)
  let getBindingPower parser token = 
    let power = parser.BindingPower.TryFind (parser.Type token)
    match power with Some power -> power | _ -> 0

  (*The current token or throws exception*)
  let current parser =  
    match !parser.Input with
    | token::_ -> token
    | _ -> unexpectedEnd ()

  (*The current Some(token) or None if empty*)
  let currentTry parser = 
    match !parser.Input with
    | token::_ -> Some token
    | _ -> None

  (*The type of the current token or throws exception*)
  let currentType parser = 
    parser |> current |> parser.Type

  (*The Some(type) of the current token or None if empty*)
  let currentTypeTry parser =
    match parser |> currentTry with
    | Some token -> Some(token |> parser.Type)
    | _ -> None

  (*Skips the current token or throws exception*)
  let skip parser =
    match !parser.Input with
    | _::input -> parser.Input := input
    | _ -> unexpectedEnd ()

  (*Skips the current token if it's type is equal to @type' or throws exception*)
  let skipIf type' parser =
    match !parser.Input with
    | token::xs when parser.Type token = type' -> 
      parser.Input := xs

    | token::_ -> 
      unexpectedToken token parser

    | _ -> unexpectedEnd ()
    
  (*Tries to skip the current token if it's type is equal to @type'*)
  let skipIfTry type' parser =
    match !parser.Input with
    | token::xs when parser.Type token = type' -> 
      parser.Input := xs
      true

    | _ -> false

  (*Gets the current token, skips it and then returns it*)
  let skipCurrent parser =
    let current = parser |> current
    parser |> skip
    current
   
  (*Gets an expression where all tokesn have a binding power greater than @rbpw*)
  let exprPwr rbpw parser =
    let rec expr left =
      match parser |> currentTry with
      | Some token when rbpw < (token |> getBindingPower parser) -> 
        parser |> skip

        let type' = parser.Type token
        let led = 
          match parser.Left.TryFind type' with
          | None -> unexpectedToken token parser
          | Some led -> led

        led token left parser |> expr

      | _ -> left

    let tok = parser |> skipCurrent
    let type' = parser.Type tok
    let nud =
      match parser.Null.TryFind type' with
      | None -> unexpectedToken tok parser
      | Some nud -> nud

    nud tok parser |> expr 
    
  (*Gets an expression where all tokens have a binding power greater than 0*)
  let expr parser = 
    parser |> exprPwr 0
    
  (*Gets an expression and then skips the next token if it's type matches @type'*)
  let exprAndSkip type' parser =
    let expr = parser |> expr
    parser |> skipIf type'
    expr
    
  (*Tries to parse the whole input as a list of expressions*)
  let rec exprList parser =
    match !parser.Input with
    | [] -> []
    | _ -> (parser |> expr) :: (parser |> exprList)
    
  (*Tries to parse the next token as a statment or an expression that ends with @term*)
  let stmt term parser =
    let token = parser |> current
    match parser.Stmt.TryFind (token |> parser.Type) with
    | Some stmt -> parser |> skip; stmt token parser
    | None -> parser |> exprAndSkip term
    
  (*Tries to parse the whole input as a list of statements or expressions ending with @term*)
  let rec stmtList term parser =
    match !parser.Input with
    | [] -> []
    | _ -> (parser |> stmt term) :: (parser |> stmtList term)

  (*
    Convenience functions exposed for 
    easing parser definition and usage
  *)

  let create<'a, 'b, 'c when 'c : comparison> type' position prettyPrint = {
    Input = ref []
    Source = None
    
    Type = type'
    Position = position
    PrettyPrint = prettyPrint
    
    BindingPower = Map.empty<'c, int>
    Null = Map.empty<'c, 'a -> T<'a, 'b, 'c> -> 'b>
    Stmt = Map.empty<'c, 'a -> T<'a, 'b, 'c> -> 'b>
    Left = Map.empty<'c, 'a -> 'b -> T<'a, 'b, 'c> -> 'b>
  }
  
  let smd token funct parser = {parser with T.Stmt = parser.Stmt.Add(token, funct)}
  let nud token funct parser = {parser with T.Null = parser.Null.Add(token, funct)}
  let led token funct parser = {parser with T.Left = parser.Left.Add(token, funct)}
  let bpw token power parser = {parser with T.BindingPower = parser.BindingPower.Add(token, power)}
  
  (*Defines a left-associative infix operator*)
  let infix f type' power parser =
    let infix token left parser = 
      f token left (parser |> exprPwr power)

    parser |> bpw type' power |> led type' infix
    
  (*Defines a right-associative infix operator*)
  let infixr f type' power parser =
    let lessPower = power - 1

    let infixr token left parser = 
      f token left (parser |> exprPwr lessPower)

    parser |> bpw type' power |> led type' infixr

  (*Defines a prefix/unary operator*)
  let prefix f type' power parser =
    let prefix tok parser = 
      f tok (parser |> exprPwr power)

    parser |> nud type' prefix

  (*Defines a constant*)
  let constant symbol value p =
    p |> nud symbol (fun _ _ -> value)
    
  (*  
    Runs the parser and treats all 
    top level construct as expressions 
  *)
  let runExpr input source parser =
    {parser with 
      T.Input = ref input
      T.Source = source
    } |> exprList
    
  (*  
    Runs the parser and treats all 
    top level construct as statements 
  *)
  let runStmt input source term parser =
    {parser with 
      T.Input = ref input
      T.Source = source
    } |> stmtList term
