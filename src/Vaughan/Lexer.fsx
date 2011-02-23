type Buffer = System.Text.StringBuilder

let ifNot a (c:char) (b:Buffer) = 
  if a <> c 
    then b.Append(c) |> ignore; true
    else false

let startWith a c (_:Buffer) = a = c
let escape = '\\', fun (c:char) -> c
let lexString = startWith '"', ifNot '"', escape

let lex lexers (input:string) =
  let i = ref 0
  let ln = input.Length-1
  let b = new Buffer(1024)
  let out = ref []
    
  while !i < ln do
    for lexer in lexers do
      let s, c, e = lexer

      if s input.[!i] b then
        i := !i + 1
          
        while c input.[!i] b do
          i := !i + 1

        out := b.ToString() :: !out
        b.Clear() |> ignore
        
  !out |> List.rev

let lexer = [lexString]
let result = lex lexer "\"foo\"\"bar\""