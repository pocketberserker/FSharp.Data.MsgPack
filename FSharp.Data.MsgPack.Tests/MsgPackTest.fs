namespace FSharp.Data

open NUnit.Framework
open FsUnit
open MsgPackValue

[<TestFixture>]
module MsgPackTest =

  open Limited

  [<Test>]
  let ``unpack nil`` () =
    [| 0xc0uy |]
    |> MsgPack.unpack |> should equal (Some Nil)
  
  [<Test>]
  let ``unpack false`` () =
    [| 0xc2uy |]
    |> MsgPack.unpack |> should equal (Some False)

  [<Test>]
  let ``unpack true`` () =
    [| 0xc3uy |]
    |> MsgPack.unpack |> should equal (Some True)

  [<Test>]
  let ``unpack positive fix int`` () =
    [| 0x7fuy |]
    |> MsgPack.unpack |> should equal (Some (UInt8 127uy))

  [<Test>]
  let ``unpack uint8`` () =
    [| 0xccuy; 0xffuy |]
    |> MsgPack.unpack |> should equal (Some (UInt8 255uy))

  [<Test>]
  let ``unpack uint16`` () =
    [| 0xcduy; 0xffuy; 0xffuy |]
    |> MsgPack.unpack |> should equal (Some (UInt16 System.UInt16.MaxValue))

  [<Test>]
  let ``unpack uint32`` () =
    [| 0xceuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy |]
    |> MsgPack.unpack |> should equal (Some (UInt32 System.UInt32.MaxValue))

  [<Test>]
  let ``unpack uint64`` () =
    [| 0xcfuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy |]
    |> MsgPack.unpack |> should equal (Some (UInt64 System.UInt64.MaxValue))

  [<Test>]
  let ``unpack negative fix int`` () =
    [| 0xffuy |]
    |> MsgPack.unpack |> should equal (Some (Int8 31y))

  [<Test>]
  let ``unpack int8`` () =
    [| 0xd0uy; 0x7fuy |]
    |> MsgPack.unpack |> should equal (Some (Int8 System.SByte.MaxValue))

  [<Test>]
  let ``unpack int16`` () =
    [| 0xd1uy; 0xffuy; 0x7fuy |]
    |> MsgPack.unpack |> should equal (Some (Int16 System.Int16.MaxValue))

  [<Test>]
  let ``unpack int32`` () =
    [| 0xd2uy; 0xffuy; 0xffuy; 0xffuy; 0x7fuy |]
    |> MsgPack.unpack |> should equal (Some (Int32 System.Int32.MaxValue))

  [<Test>]
  let ``unpack int64`` () =
    [| 0xd3uy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0x7fuy |]
    |> MsgPack.unpack |> should equal (Some (Int64 System.Int64.MaxValue))

  [<Test>]
  let ``unpack float32`` () =
    [| 0xcauy; 0xffuy; 0xffuy; 0x7fuy; 0x7fuy |]
    |> MsgPack.unpack |> should equal (Some (Float32 System.Single.MaxValue))

  [<Test>]
  let ``unpack float64`` () =
    [| 0xcbuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xefuy; 0x7fuy |]
    |> MsgPack.unpack |> should equal (Some (Float64 System.Double.MaxValue))

  let genString n str = System.String(str, n)
  let toBytes s = Microsoft.FSharp.Collections.Array.map byte s

  [<Test>]
  let ``unpack fix string`` () =
    let expected = genString 31 'A'
    [| yield 0xbfuy; yield! System.Text.Encoding.UTF8.GetBytes(expected) |]
    |> MsgPack.unpack |> should equal (Some (String expected))

  [<Test>]
  let ``unpack string 8`` () =
    let expected = genString 255 'A'
    [| yield 0xd9uy; yield 0xffuy; yield! System.Text.Encoding.UTF8.GetBytes(expected) |]
    |> MsgPack.unpack |> should equal (Some (String expected))

  [<Test>]
  let ``unpack string 16`` () =
    let length = 1000us
    let expected = genString (int length) 'A'
    [|
      yield 0xdauy
      yield! System.BitConverter.GetBytes(length)
      yield! System.Text.Encoding.UTF8.GetBytes(expected) |]
    |> MsgPack.unpack |> should equal (Some (String expected))

  [<Test>]
  let ``unpack string 32`` () =
    let length = 70000u
    let expected = genString (int length) 'A'
    [|
      yield 0xdbuy
      yield! System.BitConverter.GetBytes(length)
      yield! System.Text.Encoding.UTF8.GetBytes(expected) |]
    |> MsgPack.unpack |> should equal (Some (String expected))

  let genBin n = Microsoft.FSharp.Collections.Array.create n 0xffuy

  [<Test>]
  let ``unpack binary 8`` () =
    let expected = genBin 255
    [| yield 0xc4uy; yield 0xffuy; yield! expected |]
    |> MsgPack.unpack |> should equal (Some (Binary expected))

  [<Test>]
  let ``unpack binary 16`` () =
    let length = 1000us
    let expected = genBin (int length)
    [|
      yield 0xc5uy
      yield! System.BitConverter.GetBytes(length)
      yield! expected |]
    |> MsgPack.unpack |> should equal (Some (Binary expected))

  [<Test>]
  let ``unpack binary 32`` () =
    let length = 70000u
    let expected = genBin (int length)
    [|
      yield 0xc6uy
      yield! System.BitConverter.GetBytes(length)
      yield! expected |]
    |> MsgPack.unpack |> should equal (Some (Binary expected))

  [<Test>]
  let ``unpack fix array`` () =
    let expected = [| UInt8 1uy; UInt8 2uy; UInt8 3uy |]
    [| 0x93uy; 0x01uy; 0x02uy; 0x03uy |]
    |> MsgPack.unpack |> should equal (Some (Array expected))

  [<Test>]
  let ``unpack array16`` () =
    let length = 16
    let input = Microsoft.FSharp.Collections.Array.create length 0uy
    let expected = Microsoft.FSharp.Collections.Array.map UInt8 input
    [| yield 0xdcuy; yield! System.BitConverter.GetBytes(length); yield! input |]
    |> MsgPack.unpack |> should equal (Some (Array expected))

  [<Test>]
  let ``unpack fix map`` () =
    let expected = [ (String "compact", True); (String "schema", UInt8 0uy) ] |> Microsoft.FSharp.Collections.Map.ofList
    [|
      yield 0x82uy
      yield 0xa7uy
      yield! System.Text.Encoding.UTF8.GetBytes("compact")
      yield 0xc3uy
      yield 0xa6uy
      yield! System.Text.Encoding.UTF8.GetBytes("schema")
      yield 0x00uy |]
    |> MsgPack.unpack |> should equal (Some (Map expected))


  [<Test>]
  let ``unpack map 16`` () =
    let length = 16
    let data = [ for _ in 0 .. length do yield ("schema", True) ]
    let expected = data |> Microsoft.FSharp.Collections.List.map (fun (x,y) -> (String x, y)) |> Microsoft.FSharp.Collections.Map.ofList
    [|
      yield 0xdeuy
      yield! System.BitConverter.GetBytes(uint16 length)
      yield! data |> Microsoft.FSharp.Collections.Seq.collect (fun (x,y) ->
        [|
          yield 0xa6uy
          yield! System.Text.Encoding.UTF8.GetBytes("schema")
          yield 0xc3uy |])
    |]
    |> MsgPack.unpack |> should equal (Some (Map expected))