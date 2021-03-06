﻿namespace FSharp.Data

open System.Text
open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase

module internal MsgPackParser =

  open MsgPackValue

  let private binParser = BinParser<unit>(id)

  let private matchHead b = satisfy ((=) [| b |]) (binParser.byteN 1)

  let nil<'T when 'T : comparison> =
    matchHead HeadByte.Nil
    |>>% (Nil: MsgPackValue<'T>)

  let false'<'T when 'T : comparison> =
    matchHead HeadByte.False
    |>>% (MBool false: MsgPackValue<'T>)
  let true'<'T when 'T : comparison> =
    matchHead HeadByte.True
    |>>% (MBool true: MsgPackValue<'T>)
  let bool'<'T when 'T : comparison> = false'<'T> <|> true'<'T>

  let positiveFixInt<'T when 'T : comparison> =
    satisfy (function | One -> false | Zero -> true) binParser.bit1
    >>. binParser.bitsN 7
    |>> binParser.bitsToInt
    |>> (byte >> (MUInt8: byte -> MsgPackValue<'T>))

  let negativeFixInt<'T when 'T : comparison> =
    satisfy (function | [| One; One; One |] -> true | _ -> false) (binParser.bitsN 3)
    >>. binParser.bitsN 5
    |>> binParser.bitsToInt
    |>> (fun x -> MInt8 (int8 x - 0b100000y) : MsgPackValue<'T>)

  let uint8'<'T when 'T : comparison> =
    matchHead HeadByte.UInt8
    >>. binParser.byte1
    |>> (MUInt8: byte -> MsgPackValue<'T>)
  let uint16'<'T when 'T : comparison> =
    matchHead HeadByte.UInt16
    >>. binParser.uint16
    |>> (MUInt16: uint16 -> MsgPackValue<'T>)
  let uint32'<'T when 'T : comparison> =
    matchHead HeadByte.UInt32
    >>. binParser.uint32
    |>> (MUInt32: uint32 -> MsgPackValue<'T>)
  let uint64'<'T when 'T : comparison> =
    matchHead HeadByte.UInt64
    >>. binParser.uint64
    |>> (MUInt64: uint64 -> MsgPackValue<'T>)
  let uint'<'T when 'T : comparison> =
    choice [
      uint8'<'T>
      uint16'<'T>
      uint32'<'T>
      uint64'<'T>
    ]

  let int8'<'T when 'T : comparison> =
    matchHead HeadByte.Int8
    >>. binParser.byte1
    |>> (int8 >> (MInt8: int8 -> MsgPackValue<'T>))
  let int16'<'T when 'T : comparison> =
    matchHead HeadByte.Int16
    >>. binParser.int16
    |>> (MInt16: int16 -> MsgPackValue<'T>)
  let int32'<'T when 'T : comparison> =
    matchHead HeadByte.Int32
    >>. binParser.int32
    |>> (MInt32: int -> MsgPackValue<'T>)
  let int64'<'T when 'T : comparison> =
    matchHead HeadByte.Int64
    >>. binParser.int64
    |>> (MInt64: int64 -> MsgPackValue<'T>)
  let int'<'T when 'T : comparison> =
    choice [
      int8'<'T>
      int16'<'T>
      int32'<'T>
      int64'<'T>
    ]

  let float32'<'T when 'T : comparison> =
    matchHead HeadByte.Float32
    >>. binParser.floatP
    |>> (MFloat32: float32 -> MsgPackValue<'T>)
  let float64'<'T when 'T : comparison> =
    matchHead HeadByte.Float64
    >>. binParser.byteN 8
    |>> (fun xs -> MFloat64 (System.BitConverter.ToDouble(xs, 0)): MsgPackValue<'T>)
  let float'<'T when 'T : comparison> = float32'<'T> <|> float64'<'T>

  let private stringN<'T when 'T : comparison> matchHead length =
    matchHead
    >>. length
    >>= fun length -> binParser.byteN length
    |>> fun raw -> Encoding.UTF8.GetString(raw)
    |>> (MString: string -> MsgPackValue<'T>)
  
  let fixRaw<'T,'U when 'T : comparison> convert (value: 'U -> MsgPackValue<'T>) =
    binParser.byte1
    |> satisfy (byteToBitArray >> Array.toList >> function
    | [ One; Zero; One; _; _; _; _; _ ] -> true
    | _ -> false)
    |>> (byteToBitArray >> Seq.skip 3 >> Seq.toArray >> binParser.bitsToInt)
    >>= binParser.byteN
    |>> convert
    |>> value

  let fixString<'T when 'T : comparison> =
    fixRaw<'T, string> (fun xs -> Encoding.UTF8.GetString(xs)) MString

  let string8<'T when 'T : comparison> =
    stringN<'T> (matchHead HeadByte.String8) (binParser.byte1 |>> binParser.byteToInt)
  let string16<'T when 'T : comparison> =
    stringN<'T> (matchHead HeadByte.String16) (binParser.byte2 |>> (binParser.toUInt16 >> int))
  let string32<'T when 'T : comparison> =
    stringN<'T> (matchHead HeadByte.String32) (binParser.byte4 |>> (binParser.toUInt32 >> int))
  let string'<'T when 'T : comparison> =
    choice [
      fixString<'T>
      string8<'T>
      string16<'T>
      string32<'T>
    ]

  let private binN<'T when 'T : comparison> matchHead length =
    matchHead
    >>. length
    >>= binParser.byteN
    |>> (Binary: byte [] -> MsgPackValue<'T>)

  let bin8<'T when 'T : comparison> =
    binN<'T> (matchHead HeadByte.Binary8) (binParser.byte1 |>> binParser.byteToInt)
  let bin16<'T when 'T : comparison> =
    binN<'T> (matchHead HeadByte.Binary16) (binParser.byte2 |>> (binParser.toUInt16 >> int))
  let bin32<'T when 'T : comparison> =
    binN<'T> (matchHead HeadByte.Binary32) (binParser.byte4 |>> (binParser.toUInt32 >> int))

  let binary<'T when 'T : comparison> =
    choice [
      bin8<'T>
      bin16<'T>
      bin32<'T>
    ]

  let private fixExtN<'T when 'T : comparison> head data (f: TypeCode -> byte [] -> ExtendedValue<'T>) =
    matchHead head
    >>. binParser.byte1
    >>= fun t -> data |>> f t
    |>> Extended

  let fixExt1<'T when 'T : comparison> f = fixExtN<'T> HeadByte.FixExtended1 (binParser.byteN 1) f
  let fixExt2<'T when 'T : comparison> f = fixExtN<'T> HeadByte.FixExtended2 binParser.byte2 f
  let fixExt4<'T when 'T : comparison> f = fixExtN<'T> HeadByte.FixExtended4 binParser.byte4 f
  let fixExt8<'T when 'T : comparison> f = fixExtN<'T> HeadByte.FixExtended8 (binParser.byteN 8) f
  let fixExt16<'T when 'T : comparison> f = fixExtN<'T> HeadByte.FixExtended16 (binParser.byteN 16) f

  let private extN<'T when 'T : comparison> head length (f: TypeCode -> byte [] -> ExtendedValue<'T>) =
    matchHead head
    >>. length
    >>= fun length -> binParser.byte1 >>= fun t -> binParser.byteN length |>> f t
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

  let simpleFixType<'T when 'T : comparison> =
    positiveFixInt<'T> <|> negativeFixInt<'T>
    |> binParser.makeBitP (binParser.byteN 1)

  let rec fixArray f =
    binParser.byte1
    |> satisfy (byteToBitArray >> Array.toList >> (function
    | [ One; Zero; Zero; One; _; _; _; _ ] -> true
    | _ -> false))
    |>> (byteToBitArray >> Seq.skip 4 >> Seq.toArray >> binParser.bitsToInt)
    >>= fun size ->
      if size > 0 then manyN size (parser f)
      else preturn []
    |>> (Array.ofList >> MArray)

  and array16 f =
    matchHead HeadByte.Array16
    >>. binParser.byte2
    |>> (binParser.toUInt16 >> int)
    >>= fun size -> manyN size (parser f) |>> (Array.ofList >> MArray)

  and array32 f =
    matchHead HeadByte.Array32
    >>. binParser.byte4
    |>> (binParser.toUInt32 >> int)
    >>= fun size -> manyN size (parser f) |>> (Array.ofList >> MArray)

  and array' f =
    choice [
      fixArray f
      array16 f
      array32 f
    ]

  and keyValuePair f = parser f .>>. parser f

  and fixMap f =
    binParser.byte1
    |> satisfy (byteToBitArray >> Array.toList >> (function
    | [ One; Zero; Zero; Zero; _; _; _; _ ] -> true
    | _ -> false))
    |>> (byteToBitArray >> Seq.skip 4 >> Seq.toArray >> binParser.bitsToInt)
    >>= fun size ->
      if size > 0 then manyN size (keyValuePair f)
      else preturn []
    |>> (Map.ofList >> MMap)

  and map16 f =
    matchHead HeadByte.Map16
    >>. binParser.byte2
    |>> (binParser.toUInt16 >> int)
    >>= fun size -> manyN size (keyValuePair f) |>> (Map.ofList >> MMap)

  and map32 f =
    matchHead HeadByte.Map32
    >>. binParser.byte4
    |>> (binParser.toUInt32 >> int)
    >>= fun size -> manyN size (keyValuePair f) |>> (Map.ofList >> MMap)

  and map' f =
    choice [
      fixMap f
      map16 f
      map32 f
    ]

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
      simpleFixType
    ]

  let parseExt<'T when 'T : comparison> (f: TypeCode -> byte [] -> ExtendedValue<'T>) input = parser f input

  let parse input = parseExt (fun t xs -> Raw(t, xs)) input

  module OldSpec =

    let fixRaw<'T when 'T : comparison> = fixRaw<'T, byte []> id Binary

    let raw16<'T when 'T : comparison> =
      binN<'T> (matchHead HeadByte.String16) (binParser.byte2 |>> (binParser.toUInt16 >> int))
    let raw32<'T when 'T : comparison> =
      binN<'T> (matchHead HeadByte.String32) (binParser.byte4 |>> (binParser.toUInt32 >> int))
    let raw<'T when 'T : comparison> =
      choice [
        fixRaw<'T>
        raw16<'T>
        raw32<'T>
      ]

    let parser<'T when 'T : comparison> (f: TypeCode -> byte [] -> ExtendedValue<'T>) =
      choice [
        nil
        bool'
        uint'
        int'
        float'
        raw
        array' f
        map' f
        simpleFixType
      ]

    let parse input = parser (fun t xs -> Raw(t, xs)) input
