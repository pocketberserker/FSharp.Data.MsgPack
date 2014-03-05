namespace FSharp.Data

open System.IO
open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase

module MsgPack =

  let unpack (bin: byte seq) =
    use stream = new MemoryStream(Array.ofSeq bin)
    let input = makeBinStream stream
    match MsgPackParser.parser input with
    | (result, _) -> result
