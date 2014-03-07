namespace FSharp.Data

open System.Text
open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase

module internal MsgPackParser =

  open MsgPackValue

  let private binParser = BinParser<unit>(id)

  let private matchHead b = satisfy ((=) [| b |]) (binParser.byteN 1)

  let nil<'T when 'T : comparison> = matchHead HeadByte.Nil |>> konst (Nil: MsgPackValue<'T>)

  let false'<'T when 'T : comparison> = matchHead HeadByte.False |>> konst (Boolean false: MsgPackValue<'T>)
  let true'<'T when 'T : comparison> = matchHead HeadByte.True |>> konst (Boolean true: MsgPackValue<'T>)
  let bool'<'T when 'T : comparison> = false'<'T> <|> true'<'T>

  let positiveFixInt<'T when 'T : comparison> =
    satisfy (function | One -> false | Zero -> true) binParser.bit1
    >>. binParser.bitsN 7
    |>> binParser.bitsToInt
    |>> (byte >> (UInt8: byte -> MsgPackValue<'T>))
    |> binParser.makeBitP (binParser.byteN 1)

  let negativeFixInt<'T when 'T : comparison> =
    satisfy (function | [| One; One; One |] -> true | _ -> false) (binParser.bitsN 3)
    >>. binParser.bitsN 5
    |>> binParser.bitsToInt
    |>> (int8 >> (Int8: int8 -> MsgPackValue<'T>))
    |> binParser.makeBitP (binParser.byteN 1)

  let uint8'<'T when 'T : comparison> = matchHead HeadByte.UInt8 >>. binParser.byte1 |>> (UInt8: byte -> MsgPackValue<'T>)
  let uint16'<'T when 'T : comparison> = matchHead HeadByte.UInt16 >>. binParser.uint16 |>> (UInt16: uint16 -> MsgPackValue<'T>)
  let uint32'<'T when 'T : comparison> = matchHead HeadByte.UInt32 >>. binParser.uint32 |>> (UInt32: uint32 -> MsgPackValue<'T>)
  let uint64'<'T when 'T : comparison> = matchHead HeadByte.UInt64 >>. binParser.uint64 |>> (UInt64: uint64 -> MsgPackValue<'T>)
  let uint'<'T when 'T : comparison> =
    choice [
      positiveFixInt<'T>
      uint8'<'T>
      uint16'<'T>
      uint32'<'T>
      uint64'<'T>
    ]

  let int8'<'T when 'T : comparison> = matchHead HeadByte.Int8 >>. binParser.byte1 |>> (int8 >> (Int8: int8 -> MsgPackValue<'T>))
  let int16'<'T when 'T : comparison> = matchHead HeadByte.Int16 >>. binParser.int16 |>> (Int16: int16 -> MsgPackValue<'T>)
  let int32'<'T when 'T : comparison> = matchHead HeadByte.Int32 >>. binParser.int32 |>> (Int32: int -> MsgPackValue<'T>)
  let int64'<'T when 'T : comparison> = matchHead HeadByte.Int64 >>. binParser.int64 |>> (Int64: int64 -> MsgPackValue<'T>)
  let int'<'T when 'T : comparison> =
    choice [
      negativeFixInt<'T>
      int8'<'T>
      int16'<'T>
      int32'<'T>
      int64'<'T>
    ]

  let float32'<'T when 'T : comparison> =
    matchHead HeadByte.Float32 >>. binParser.floatP |>> (Float32: float32 -> MsgPackValue<'T>)
  let float64'<'T when 'T : comparison> =
    matchHead HeadByte.Float64 >>. binParser.byteN 8
    |>> (fun xs -> Float64 (System.BitConverter.ToDouble(xs, 0)): MsgPackValue<'T>)
  let float'<'T when 'T : comparison> = float32'<'T> <|> float64'<'T>

  let private stringN<'T when 'T : comparison> matchHead length =
    matchHead
    >>. length
    >>= fun length -> binParser.byteN length
    |>> fun raw -> Encoding.UTF8.GetString(raw)
    |>> (String: string -> MsgPackValue<'T>)
  
  let fixString<'T when 'T : comparison> =
    let matchHead =
      satisfy (function | [| One; Zero; One |] -> true | _ -> false) (binParser.bitsN 3)
      |> binParser.makeBitP (binParser.byteN 1)
      |>> (konst [| 0b101uy |])
    let length = binParser.bitsN 5 |>> binParser.bitsToInt |> binParser.makeBitP (binParser.byteN 1)
    stringN<'T> matchHead length

  let string8<'T when 'T : comparison> =
    stringN<'T> (matchHead HeadByte.String8) (binParser.byte1 |>> binParser.byteToInt)
  let string16<'T when 'T : comparison> =
    stringN<'T> (matchHead HeadByte.String16) (binParser.byte2 |>> (binParser.toUInt16 >> int))
  let string32<'T when 'T : comparison> =
    stringN<'T> (matchHead HeadByte.String32) (binParser.byte4 |>> (binParser.toUInt32 >> int))
  let string'<'T when 'T : comparison> = choice [ fixString<'T>; string8<'T>; string16<'T>; string32<'T> ]

  let private binN<'T when 'T : comparison> matchHead length =
    matchHead
    >>. length
    >>= fun length -> binParser.byteN length
    |>> (Binary: byte [] -> MsgPackValue<'T>)

  let bin8<'T when 'T : comparison> =
    binN<'T> (matchHead HeadByte.Binary8) (binParser.byte1 |>> binParser.byteToInt)
  let bin16<'T when 'T : comparison> =
    binN<'T> (matchHead HeadByte.Binary16) (binParser.byte2 |>> (binParser.toUInt16 >> int))
  let bin32<'T when 'T : comparison> =
    binN<'T> (matchHead HeadByte.Binary32) (binParser.byte4 |>> (binParser.toUInt32 >> int))

  let binary<'T when 'T : comparison> = choice [ bin8<'T>; bin16<'T>; bin32<'T> ]

  let private fixExtN<'T when 'T : comparison> head data (f: TypeCode -> byte [] -> ExtendedValue<'T>) =
    matchHead head
    >>. binParser.byte1
    >>= fun t -> data |>> fun xs -> f t xs
    |>> Extended

  let fixExt1<'T when 'T : comparison> f = fixExtN<'T> HeadByte.FixExtended1 (binParser.byteN 1) f
  let fixExt2<'T when 'T : comparison> f = fixExtN<'T> HeadByte.FixExtended2 binParser.byte2 f
  let fixExt4<'T when 'T : comparison> f = fixExtN<'T> HeadByte.FixExtended4 binParser.byte4 f
  let fixExt8<'T when 'T : comparison> f = fixExtN<'T> HeadByte.FixExtended8 (binParser.byteN 8) f
  let fixExt16<'T when 'T : comparison> f = fixExtN<'T> HeadByte.FixExtended16 (binParser.byteN 16) f

  let private extN<'T when 'T : comparison> head length (f: TypeCode -> byte [] -> ExtendedValue<'T>) =
    matchHead head
    >>. length
    >>= fun length -> binParser.byte1 >>= fun t -> binParser.byteN length |>> fun xs -> f t xs
    |>> Extended

  let ext8<'T when 'T : comparison> (f: TypeCode -> byte [] -> ExtendedValue<'T>) =
    extN<'T> HeadByte.Extended8 (binParser.byte1 |>> binParser.byteToInt) f
  let ext16<'T when 'T : comparison> (f: TypeCode -> byte [] -> ExtendedValue<'T>) =
    extN<'T> HeadByte.Extended16 (binParser.byte2 |>> (binParser.toUInt16 >> int)) f
  let ext32<'T when 'T : comparison> (f: TypeCode -> byte [] -> ExtendedValue<'T>) =
    extN<'T> HeadByte.Extended32 (binParser.byte4 |>> (binParser.toUInt32 >> int)) f

  let ext f =
    choice [
      fixExt1 f
      fixExt2 f
      fixExt4 f
      fixExt8 f
      fixExt16 f
      ext8 f
      ext16 f
      ext32 f
    ]

  let rec fixArray f =
    satisfy (function | [| One; Zero; Zero; One |] -> true | _ -> false) (binParser.bitsN 4)
    >>. binParser.bitsN 4 |>> binParser.bitsToInt
    |> binParser.makeBitP (binParser.byteN 1)
    >>= fun size -> manyN size (parser f) |>> (Array.ofList >> Array)

  and array16 f =
    matchHead HeadByte.Array16
    >>. binParser.byte2
    |>> (binParser.toUInt16 >> int)
    >>= fun size -> manyN size (parser f) |>> (Array.ofList >> Array)

  and array32 f =
    matchHead HeadByte.Array32
    >>. binParser.byte4
    |>> (binParser.toUInt32 >> int)
    >>= fun size -> manyN size (parser f) |>> (Array.ofList >> Array)

  and array' f = fixArray f <|> array16 f <|> array32 f

  and keyValuePair f = parser f >>= fun key -> parser f |>> fun value -> (key, value)

  and fixMap f =
    satisfy (function | [| One; Zero; Zero; Zero |] -> true | _ -> false) (binParser.bitsN 4)
    >>. binParser.bitsN 4 |>> binParser.bitsToInt
    |> binParser.makeBitP (binParser.byteN 1)
    >>= fun size -> manyN size (keyValuePair f) |>> (Map.ofList >> Map)

  and map16 f =
    matchHead HeadByte.Map16
    >>. binParser.byte2
    |>> (binParser.toUInt16 >> int)
    >>= fun size -> manyN size (keyValuePair f) |>> (Map.ofList >> Map)

  and map32 f =
    matchHead HeadByte.Map32
    >>. binParser.byte4
    |>> (binParser.toUInt32 >> int)
    >>= fun size -> manyN size (keyValuePair f) |>> (Map.ofList >> Map)

  and map' f = fixMap f <|> map16 f <|> map32 f

  and parser<'T when 'T : comparison> (f: TypeCode -> byte [] -> ExtendedValue<'T>) =
    choice [
      nil
      bool'
      uint'
      int'
      float'
      string'
      binary
      array' f
      map' f
      ext f
    ]

  let parseExt<'T when 'T : comparison> (f: TypeCode -> byte [] -> ExtendedValue<'T>) input = parser f input

  let parse input = parseExt (fun t xs -> Raw (t, xs)) input
