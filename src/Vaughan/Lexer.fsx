

let isNumber c = c >= '0' && c <= '9'
let isStringStart c = c = '"'
let isNotStringEnd c = c <> '"'
let stringDone s = Some("#string", s)

type Result
  = Char of char
  | Parser of (char -> Result)
  | Failed
  | Stop
  
let rec lexString = function 
  | '"' -> Parser lexStringInner
  | _ -> Failed

and lexStringInner = function
  | '\\' -> Parser lexEscapedString
  | c -> Char c

and lexEscapedString = function
  | c -> Char c

type Lexer<'a>
  = Start of (char -> bool)
  | While of (char -> bool)
  | Match of (string -> bool)
  | Done of (string -> 'a option)

let lex (lexers:Lexer<'a> list list) (input:string) =
  let i = ref 0
  let buffer = new System.Text.StringBuilder(32)

  let rec lex lexer =
    match lexer with
    | [] -> None
    | Start test::lexers ->
      if test input.[!i] then 
        buffer.Append(input.[!i]) |> ignore
        i := !i + 1
        lex lexers

      else  
        None

    | While test::lexers ->
      while test input.[!i] do
        buffer.Append(input.[!i]) |> ignore
        i := !i + 1

      lex lexers

    | Done done'::lexers ->
      done' (buffer.ToString())

    | Match _::lexers -> None

  let tokens = System.Collections.Generic.List<'a>()

  while !i < input.Length-1 do
    let isDone = ref false

    while !isDone |> not do

      for lexer in lexers do
        match lex lexer with
        | None -> ()
        | Some token ->
          tokens.Add(token)
          isDone := true

    isDone := false
    buffer.Clear() |> ignore

  List.ofSeq tokens

let lexer = [
  [Start isStringStart; While isNotStringEnd; Done stringDone]
]

let result = lex lexer "\"foo\"\"bar\""