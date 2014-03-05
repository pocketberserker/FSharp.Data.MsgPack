namespace FSharp.Data

[<AutoOpen>]
module internal Prelude =

  let inline konst a = fun _ -> a