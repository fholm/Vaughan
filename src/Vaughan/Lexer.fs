namespace Vaughan

module Lexer =
  
  type Exn(msg, pos) =  
    inherit System.Exception(msg)
    member x.Position = pos

  let exn msg = Exn(msg, (0, 0)) |> raise

  let private unexpectedEnd() = "Unexpected end of input"|>exn
  let private alreadyAtFirst() = "Already at first character"|>exn

  type Input = 
    (unit -> char) *
    (unit -> unit) *
    (unit -> unit)

  let input (chars:string) : (unit -> bool) * Input =
    let i = ref 0
    let max = chars.Length-1

    let onlast() = !i >= max
    let current() = chars.[!i]
    let forward() = if !i<max then i := !i+1 else unexpectedEnd()
    let rewind() = if !i>0 then i := !i-1 else alreadyAtFirst()

    onlast, (current, forward, rewind)

  type T<'a> = {
    Input : Input
    Lexers : (char -> bool * Input -> 'a) list
  }
  
  let current (current, _, _) : char = current()
  let forward (_, forward, _) : unit = forward()
  let rewind (_, _, rewind) : unit = rewind()

  let isNot c (current, _, _) = current() <> c 
  let buffer () = new System.Text.StringBuilder()


  let isNumber c = c >= '0' && c <= '9'
  let isStringStart c = c = '"'
  let isNotStringEnd c = c <> '"'
  let stringDone s = ("#string", s)
  
  type Lexer<'a>
    = Start of (char -> bool)
    | While of (char -> bool)
    | Match of (string -> bool)
    | Done of (string -> 'a)
  
  let lexer = [
    [Start isNumber; While isNotStringEnd; Done stringDone]
  ]
