namespace FSharp.Data

module internal MsgPackFormatter =

  open MsgPackValue

  let (|FixExt1|FixExt2|FixExt4|FixExt8|FixExt16|Other|) value =
    let length = Array.length value
    if length = 1 then FixExt1 HeadByte.FixExtended1
    elif length = 2 then FixExt2 HeadByte.FixExtended2
    elif length = 4 then FixExt4 HeadByte.FixExtended4
    elif length = 8 then FixExt8 HeadByte.FixExtended8
    elif length = 16 then FixExt16 HeadByte.FixExtended16
    else Other

  let (|Ext8|Ext16|Ext32|) value =
    let length = Array.length value
    if length <= int System.Byte.MaxValue then Ext8(HeadByte.Extended8, intToBytes length)
    elif length <= int System.UInt16.MaxValue then Ext16(HeadByte.Extended16, intToBytes length)
    else Ext32(HeadByte.Extended32, intToBytes length)

  let rec formatExtended = function
    | Raw(code, value) ->
      match value with
      | FixExt1 head
      | FixExt2 head
      | FixExt4 head
      | FixExt8 head
      | FixExt16 head -> [| yield head; yield code; yield! value |]
      | Ext8(head, length)
      | Ext16(head, length)
      | Ext32(head, length) ->
        [| yield head; yield! length; yield code; yield! value |]
    | Parsed (value: #IPackable) -> value.Pack()

  let rec format = function
    | Nil -> [| HeadByte.Nil |]
    | MBool false -> [| HeadByte.False |]
    | MBool true -> [| HeadByte.True |]
    | MUInt8 value when value <= 0x7fuy -> [| value |]
    | MUInt8 value -> [| HeadByte.UInt8; value |]
    | MUInt16 value -> [| yield HeadByte.UInt16; yield! uint16ToBytes value |]
    | MUInt32 value -> [| yield HeadByte.UInt32; yield! uint32ToBytes value |]
    | MUInt64 value -> [| yield HeadByte.UInt64; yield! uint64ToBytes value |]
    | MInt8 value when value < 0y && value > -32y -> [| byte value |]
    | MInt8 value -> [| HeadByte.Int8; byte value |]
    | MInt16 value -> [| yield HeadByte.Int16; yield! int16ToBytes value |]
    | MInt32 value -> [| yield HeadByte.Int32; yield! intToBytes value |]
    | MInt64 value -> [| yield HeadByte.Int64; yield! int64ToBytes value |]
    | MFloat32 value -> [| yield HeadByte.Float32; yield! float32ToBytes value |]
    | MFloat64 value -> [| yield HeadByte.Float64; yield! floatToBytes value |]
    | MString value ->
      let length = String.length value
      let value = stringToBytes value
      if length <= 31 then [| yield 0xa0uy + byte length; yield! value |]
      elif length <= int System.Byte.MaxValue then
        [| yield HeadByte.String8; yield byte length; yield! value |]
      elif length <= int System.UInt16.MaxValue then
        [| yield HeadByte.String16; yield! length |> uint16 |> uint16ToBytes; yield! value |]
      else [| yield HeadByte.String32; yield! length |> uint32 |> uint32ToBytes; yield! value |]
    | Binary value ->
      let length = Array.length value
      if length <= int System.Byte.MaxValue then
        [| yield HeadByte.Binary8; yield byte length; yield! value |]
      elif length <= int System.UInt16.MaxValue then
        [| yield HeadByte.Binary16; yield! length |> uint16 |> uint16ToBytes; yield! value |]
      else [| yield HeadByte.Binary32; yield! length |> uint32 |> uint32ToBytes; yield! value |]
    | MArray value ->
      let length = Array.length value
      let values = Array.collect format value
      if length <= 15 then [| yield 0x90uy + byte length; yield! values |]
      elif length <= int System.UInt16.MaxValue then
        [| yield HeadByte.Array16; yield! length |> uint16 |> uint16ToBytes; yield! values |]
      else [| yield HeadByte.Array32; yield! length |> uint32 |> uint32ToBytes; yield! values |]
    | MMap values ->
      let length = values.Count
      let values =
        values
        |> Map.fold (fun acc k v -> Array.append acc [| yield! format k; yield! format v |]) [||]
      if length <= 15 then [| yield 0x80uy + byte length; yield! values |]
      elif length <= int System.UInt16.MaxValue then
        [| yield HeadByte.Map16; yield! length |> uint16 |> uint16ToBytes; yield! values |]
      else [| yield HeadByte.Map32; yield! length |> uint32 |> uint32ToBytes; yield! values |]
    | Extended ext -> formatExtended ext

  module OldSpec =

    let formatRaw length value =
      if length <= 31 then [| yield 0xa0uy + byte length; yield! value |]
      elif length <= int System.UInt16.MaxValue then
        [| yield HeadByte.String16; yield! length |> uint16 |> uint16ToBytes; yield! value |]
      else [| yield HeadByte.String32; yield! length |> uint32 |> uint32ToBytes; yield! value |]

    let format (value: MsgPackValue) =
      match value with
      | MString value -> formatRaw (String.length value) (stringToBytes value)
      | Binary value -> formatRaw (Array.length value) value
      | _ -> format value
