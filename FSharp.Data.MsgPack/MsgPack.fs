namespace FSharp.Data

open System.IO
open ParsecClone.BinaryCombinator

module MsgPack =

  let private unpackF parse (binary : byte []) =
    use stream = new MemoryStream(binary)
    stream |> makeBinStream |> parse |> fst

  let unpack binary : MsgPackValue option = unpackF MsgPackParser.parse binary

  let unpackExt<'T when 'T : comparison> binary convert : MsgPackValue<'T> option =
    unpackF (MsgPackParser.parseExt<'T> convert) binary

  let pack (value: MsgPackValue) = MsgPackFormatter.format value
  let packExt value = MsgPackFormatter.format value

  module Helper =

    let pack (value: #IPackable) = value.Pack()

  module OldSpec =
  
    let unpack binary : MsgPackValue option = unpackF MsgPackParser.OldSpec.parse binary

    open MsgPackValue

    let pack value = MsgPackFormatter.OldSpec.format value
