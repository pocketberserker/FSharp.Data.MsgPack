namespace FSharp.Data

open System.Text
open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase

module MsgPackParser =

  open MsgPackValue

  let private binParser = BinParser<_>(id)

  let private matchHead b = satisfy ((=) [| b |]) (binParser.byteN 1)

  let nil = matchHead HeadByte.Nil |>> konst Nil

  let false' = matchHead HeadByte.False |>> konst (Boolean false)
  let true' = matchHead HeadByte.True |>> konst (Boolean true)
  let bool' = false' <|> true'

  let positiveFixInt =
    satisfy (function | One -> false | Zero -> true) binParser.bit1
    >>. binParser.bitsN 7
    |>> binParser.bitsToInt
    |>> (byte >> UInt8)
    |> binParser.makeBitP (binParser.byteN 1)

  let negativeFixInt =
    satisfy (function | [| One; One; One |] -> true | _ -> false) (binParser.bitsN 3)
    >>. binParser.bitsN 5
    |>> binParser.bitsToInt
    |>> (int8 >> Int8)
    |> binParser.makeBitP (binParser.byteN 1)

  let uint8' = matchHead HeadByte.UInt8 >>. binParser.byte1 |>> UInt8
  let uint16' = matchHead HeadByte.UInt16 >>. binParser.uint16 |>> UInt16
  let uint32' = matchHead HeadByte.UInt32 >>. binParser.uint32 |>> UInt32
  let uint64' = matchHead HeadByte.UInt64 >>. binParser.uint64 |>> UInt64
  let uint' = positiveFixInt <|> uint8' <|> uint16' <|> uint32' <|> uint64'

  let int8' = matchHead HeadByte.Int8 >>. binParser.byte1 |>> (int8 >> Int8)
  let int16' = matchHead HeadByte.Int16 >>. binParser.int16 |>> Int16
  let int32' = matchHead HeadByte.Int32 >>. binParser.int32 |>> Int32
  let int64' = matchHead HeadByte.Int64 >>. binParser.int64 |>> Int64
  let int' = negativeFixInt <|> int8' <|> int16' <|> int32' <|> int64'

  let float32' = matchHead HeadByte.Float32 >>. binParser.floatP |>> Float32
  let float64' = matchHead HeadByte.Float64 >>. binParser.byteN 8 |>> (fun xs -> Float64 (System.BitConverter.ToDouble(xs, 0)))
  let float' = float32' <|> float64'

  let private rawN matchHead length convertLength convertType valueType =
    parse {
      do! matchHead >>. eof
      let! length = length |>> convertLength
      let! str = binParser.byteN length |>> convertType
      return valueType str
    }
  
  let private stringN matchHead length convertLength =
    rawN matchHead length convertLength (fun xs -> Encoding.UTF8.GetString(xs)) String

  let fixString =
    let matchHead =
      satisfy (function | [| One; Zero; One |] -> true | _ -> false) (binParser.bitsN 3)
      |> binParser.makeBitP (binParser.byteN 1)
    let length = binParser.bitsN 5 |>> binParser.bitsToInt |> binParser.makeBitP (binParser.byteN 1)
    stringN matchHead length id

  let string8 = stringN (matchHead HeadByte.String8) binParser.byte1 binParser.byteToInt
  let string16 = stringN (matchHead HeadByte.String16)  binParser.byte2 (binParser.toUInt16 >> int)
  let string32 = stringN (matchHead HeadByte.String32) binParser.byte4 (binParser.toUInt32 >> int)
  let string' = fixString <|> string8 <|> string16 <|> string32

  let private binN matchHead length convertLength =
    rawN matchHead length convertLength id Binary

  let bin8 = binN (matchHead HeadByte.Binary8) binParser.byte1 binParser.byteToInt
  let bin16 = binN (matchHead HeadByte.Binary16) binParser.byte2 (binParser.toUInt16 >> int)
  let bin32 = binN (matchHead HeadByte.Binary32) binParser.byte4 (binParser.toUInt32 >> int)

  let binary = bin8 <|> bin16 <|> bin32

  let private fixExtN head data =
    matchHead head
    >>. binParser.byte1
    >>= fun t -> data |>> fun xs -> (t, xs)
    |>> Extended

  let fixExt1 = fixExtN HeadByte.FixExtended1 (binParser.byteN 1)
  let fixExt2 = fixExtN HeadByte.FixExtended2 binParser.byte2
  let fixExt4 = fixExtN HeadByte.FixExtended4 binParser.byte4
  let fixExt8 = fixExtN HeadByte.FixExtended8 (binParser.byteN 8)
  let fixExt16 = fixExtN HeadByte.FixExtended16 (binParser.byteN 16)

  let private extN head length =
    matchHead head
    >>. length
    >>= fun length -> binParser.byte1 >>= fun t -> binParser.byteN length |>> fun xs -> (t, xs)
    |>> Extended

  let ext8 = extN HeadByte.Extended8 (binParser.byte1 |>> binParser.byteToInt)
  let ext16 = extN HeadByte.Extended16 (binParser.byte2 |>> (binParser.toUInt16 >> int))
  let ext32 = extN HeadByte.Extended32 (binParser.byte4 |>> (binParser.toUInt32 >> int))

  let ext = fixExt1 <|> fixExt2 <|> fixExt2 <|> fixExt4 <|> fixExt8 <|> fixExt16 <|> ext8 <|> ext16 <|> ext32

  let rec fixArray =
    satisfy (function | [| One; Zero; Zero; One |] -> true | _ -> false) (binParser.bitsN 4)
    >>. binParser.bitsN 4 |>> binParser.bitsToInt
    |> binParser.makeBitP (binParser.byteN 1)
    >>= fun size -> manyN size parser |>> (Array.ofList >> Array)

  and array16 =
    matchHead HeadByte.Array16
    >>. binParser.byte2
    |>> (binParser.toUInt16 >> int)
    >>= fun size -> manyN size parser |>> (Array.ofList >> Array)

  and array32 =
    matchHead HeadByte.Array32
    >>. binParser.byte4
    |>> (binParser.toUInt32 >> int)
    >>= fun size -> manyN size parser |>> (Array.ofList >> Array)

  and array' = fixArray <|> array16 <|> array32

  and keyValuePair = parser >>= fun key -> parser |>> fun value -> (key, value)

  and fixMap =
    satisfy (function | [| One; Zero; Zero; Zero |] -> true | _ -> false) (binParser.bitsN 4)
    >>. binParser.bitsN 4 |>> binParser.bitsToInt
    |> binParser.makeBitP (binParser.byteN 1)
    >>= fun size -> manyN size keyValuePair |>> (Map.ofList >> Map)

  and map16 =
    matchHead HeadByte.Map16
    >>. binParser.byte2
    |>> (binParser.toUInt16 >> int)
    >>= fun size -> manyN size keyValuePair |>> (Map.ofList >> Map)

  and map32 =
    matchHead HeadByte.Map32
    >>. binParser.byte4
    |>> (binParser.toUInt32 >> int)
    >>= fun size -> manyN size keyValuePair |>> (Map.ofList >> Map)

  and map' = fixMap <|> map16 <|> map32

  and parser : Parser<_,_,_,unit> =
    choice [
      nil
      bool'
      uint'
      int'
      float'
      string'
      binary
      array'
      map'
      ext
    ]
