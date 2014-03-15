namespace FSharp.Data

type TypeCode = byte

type ExtendedValue<'T when 'T : comparison> =
  | Raw of TypeCode * byte []
  | Parsed of 'T

type MsgPackValue<'T when 'T : comparison> =
  | UInt8 of uint8
  | UInt16 of uint16
  | UInt32 of uint32
  | UInt64 of uint64
  | Int8 of int8
  | Int16 of int16
  | Int32 of int
  | Int64 of int64
  | Nil
  | Boolean of bool
  | Float32 of float32
  | Float64 of float
  | String of string
  | Binary of byte []
  | Array of MsgPackValue<'T> []
  | Map of Map<MsgPackValue<'T>, MsgPackValue<'T>>
  | Extended of ExtendedValue<'T>

type IPackable =
  abstract member Code: TypeCode
  abstract member Pack: unit -> byte []

type NoneUseExtended = NoneUseExtended
  with
    interface IPackable with
      member x.Code = failwith "You should not use this member if you don't use Extended."
      member x.Pack() = failwith "You should not use this member if you don't use Extended."

type MsgPackValue = MsgPackValue<NoneUseExtended>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MsgPackValue =

  module Limited =
    
    let Nil: MsgPackValue = Nil
    let True: MsgPackValue = Boolean true
    let False: MsgPackValue = Boolean false
    let UInt8 value : MsgPackValue = UInt8 value
    let UInt16 value : MsgPackValue = UInt16 value
    let UInt32 value : MsgPackValue = UInt32 value
    let UInt64 value : MsgPackValue = UInt64 value
    let Int8 value : MsgPackValue = Int8 value
    let Int16 value : MsgPackValue = Int16 value
    let Int32 value : MsgPackValue = Int32 value
    let Int64 value : MsgPackValue = Int64 value
    let Float32 value : MsgPackValue = Float32 value
    let Float64 value : MsgPackValue = Float64 value
    let String value : MsgPackValue = String value
    let Binary value : MsgPackValue = Binary value
    let Array values : MsgPackValue = Array values
    let Map map : MsgPackValue = Map map

  module HeadByte =

    [<Literal>]
    let Nil = 0xc0uy
    [<Literal>]
    let False = 0xc2uy
    [<Literal>]
    let True = 0xc3uy
    [<Literal>]
    let Binary8 = 0xc4uy
    [<Literal>]
    let Binary16 = 0xc5uy
    [<Literal>]
    let Binary32 = 0xc6uy
    [<Literal>]
    let Extended8 = 0xc7uy
    [<Literal>]
    let Extended16 = 0xc8uy
    [<Literal>]
    let Extended32 = 0xc9uy
    [<Literal>]
    let Float32 = 0xcauy
    [<Literal>]
    let Float64 = 0xcbuy
    [<Literal>]
    let UInt8 = 0xccuy
    [<Literal>]
    let UInt16 = 0xcduy
    [<Literal>]
    let UInt32 = 0xceuy
    [<Literal>]
    let UInt64 = 0xcfuy
    [<Literal>]
    let Int8 = 0xd0uy
    [<Literal>]
    let Int16 = 0xd1uy
    [<Literal>]
    let Int32 = 0xd2uy
    [<Literal>]
    let Int64 = 0xd3uy
    [<Literal>]
    let FixExtended1 = 0xd4uy
    [<Literal>]
    let FixExtended2 = 0xd5uy
    [<Literal>]
    let FixExtended4 = 0xd6uy
    [<Literal>]
    let FixExtended8 = 0xd7uy
    [<Literal>]
    let FixExtended16 = 0xd8uy
    [<Literal>]
    let String8 = 0xd9uy
    [<Literal>]
    let String16 = 0xdauy
    [<Literal>]
    let String32 = 0xdbuy
    [<Literal>]
    let Array16 = 0xdcuy
    [<Literal>]
    let Array32 = 0xdduy
    [<Literal>]
    let Map16 = 0xdeuy
    [<Literal>]
    let Map32 = 0xdfuy
