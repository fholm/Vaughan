namespace Vaughan

module Parser = 
  
  (*
  F# implementation of a generic Top-Down-Operator-Precedence Parser 
  as described in this paper http://portal.acm.org/citation.cfm?id=512931.

  The parser has been extended to allow for statements in comparison to Pratt's
  original algorithm which only parsed languages which use expression-only grammar.

  The parsers is "impure" in the sense that is uses a ref-cell for storing the
  input in the T<_, _, _> record class, this is soley for performance reasons
  as it's to expensive to create a new record object for every consumed token.
  Certain functions also throw exceptions, which generally also is considered impure.

  The parser produces nice error message in this style:

  Error on line: 5 col: 9
  4: if(x == y) {
  5:   print'x equals y');
  ----------^
  Unexpected: #string "x equals y"

  More information:
  * http://en.wikipedia.org/wiki/Vaughan_Pratt (Original Inventor)
  * http://en.wikipedia.org/wiki/Pratt_parser (Alias name)
  * http://effbot.org/zone/simple-top-down-parsing.htm (Python implementation)
  * http://javascript.crockford.com/tdop/tdop.html (JavaScript implementation)
  *)

  type Pos = int * int

  type T<'a, 'b, 'c> when 'c : comparison = {
      Input : 'a list ref
      Lines : string option

      Type : 'a -> 'c
      Position : 'a -> Pos
      PrettyPrint : ('a -> string) option

      //Parser definitions and binding powers
      BindingPower : Map<'c, int>
      Null : Map<'c, 'a -> T<'a, 'b, 'c> -> 'b>
      Stmt : Map<'c, 'a -> T<'a, 'b, 'c> -> 'b>
      Left : Map<'c, 'a -> 'b -> T<'a, 'b, 'c> -> 'b>
  }
  
  type Pattern<'a, 'b, 'c> when 'c : comparison 
    = Sym of 'c
    | Get of (T<'a, 'b, 'c> -> 'b)

  //Errors
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
          let arrow = "-" |> stringRepeat (nrl + column)
          result := !result+nr+": "+text+"\n"+arrow+"^\n"

      !result

  let exn msg = Exn(msg, (0, 0)) |> raise
  let exnLine pos msg = 
    let line = sprintf "Error on line: %i col: %i\n" (fst pos) (snd pos)
    Exn(line + msg, pos) |> raise

  let private unexpectedEnd () = "Unexpected end of input" |> exn
  let private unexpectedToken token parser =
    let type' =
      match parser.PrettyPrint with
      | None -> (token |> parser.Type).ToString()
      | Some f -> token |> f

    let pos = token |> parser.Position
    let source = parser.Lines |> errorSource pos 
    let expected = sprintf "Unexpected: %s" type'
    (source + expected) |> exnLine pos

  let inline private getBindingPower tok parser = 
    let pwr = parser.BindingPower.TryFind (parser.Type tok)
    match pwr with Some pwr -> pwr | _ -> 0

  let current parser =  
    match !parser.Input with
    | token::_ -> token
    | _ -> unexpectedEnd ()

  let currentTry parser = 
    match !parser.Input with
    | token::_ -> Some token
    | _ -> None

  let currentType parser = 
    parser |> current |> parser.Type

  let currentTypeTry parser =
    match parser |> currentTry with
    | Some token -> Some(token |> parser.Type)
    | _ -> None

  let skip parser =
    match !parser.Input with
    | _::input -> parser.Input := input
    | _ -> unexpectedEnd ()

  let skipIf type' parser =
    match !parser.Input with
    | token::xs when parser.Type token = type' -> 
      parser.Input := xs

    | token::_ -> 
      unexpectedToken token parser

    | _ -> unexpectedEnd ()

  let skipCurrent parser =
    let current = parser |> current
    parser |> skip
    current
   
  let exprPwr rbpw parser =
    let rec expr left =
      match parser |> currentTry with
      | Some token when rbpw < (parser |> getBindingPower token) -> 
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

  let expr parser = 
    parser |> exprPwr 0

  let exprSkip type' parser =
    let expr = parser |> expr
    parser |> skipIf type'
    expr

  let rec exprList parser =
    match !parser.Input with
    | [] -> []
    | _ -> (parser |> expr) :: (parser |> exprList)

  let stmt term parser =
    let token = parser |> current
    match parser.Stmt.TryFind (token |> parser.Type) with
    | Some stmt -> parser |> skip; stmt token parser
    | None -> parser |> exprSkip term

  let rec stmtList term parser =
    match !parser.Input with
    | [] -> []
    | _ -> (parser |> stmt term) :: (parser |> stmtList term)

  let match' pattern parser =
    let rec match' acc pattern parser =
      match pattern with
      | [] -> acc |> List.rev

      | Sym(symbol)::pattern -> 
        parser |> skipIf symbol
        parser |> match' acc pattern

      | Get(f)::pattern ->
        let acc = (f parser) :: acc
        parser |> match' acc pattern 

    parser |> match' [] pattern

  (*
    Convenience functions exposed for 
    easing parser definition and usage
  *)

  let create<'a, 'b, 'c when 'c : comparison> type' position prettyPrint = {
    Input = ref []
    Lines = None
    
    Type = type'
    Position = position
    PrettyPrint = prettyPrint
    
    BindingPower = Map.empty<'c, int>
    Null = Map.empty<'c, 'a -> T<'a, 'b, 'c> -> 'b>
    Stmt = Map.empty<'c, 'a -> T<'a, 'b, 'c> -> 'b>
    Left = Map.empty<'c, 'a -> 'b -> T<'a, 'b, 'c> -> 'b>
  }
  
  let matchError () = exn "Match pattern failed"
  let smd token funct parser = {parser with T.Stmt = parser.Stmt.Add(token, funct)}
  let nud token funct parser = {parser with T.Null = parser.Null.Add(token, funct)}
  let led token funct parser = {parser with T.Left = parser.Left.Add(token, funct)}
  let bpw token power parser = {parser with T.BindingPower = parser.BindingPower.Add(token, power)}
  
  (*Defines a left-associative infix operator*)
  let infix f typ pwr p =
    let infix tok left p = 
      f tok left (p |> exprPwr pwr)

    p |> bpw typ pwr |> led typ infix
    
  (*Defines a right-associative infix operator*)
  let infixr f typ pwr p =
    let lpwr = pwr - 1

    let infix tok left p = 
      f tok left (p |> exprPwr lpwr)

    p |> bpw typ pwr |> led typ infix

  (*Defines a prefix/unary operator*)
  let prefix f typ pwr p =
    let prefix tok parser = 
      f tok (parser |> exprPwr pwr)

    p |> nud typ prefix

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
      T.Lines = source
    } |> exprList
    
  (*  
    Runs the parser and treats all 
    top level construct as statements 
  *)
  let runStmt input source term parser =
    {parser with 
      T.Input = ref input
      T.Lines = source
    } |> stmtList term