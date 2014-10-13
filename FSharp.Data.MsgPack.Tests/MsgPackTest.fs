namespace FSharp.Data.MsgPack.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open FSharp.Data
open MsgPackValue

[<TestFixture>]
module MsgPackTest =

  open Limited

  [<Test>]
  let ``nil`` () =
    Nil
    |> MsgPack.pack
    |> MsgPack.unpack
    |> should equal (Some Nil)
  
  [<Test>]
  let ``false`` () =
    False
    |> MsgPack.pack
    |> MsgPack.unpack
    |> should equal (Some False)

  [<Test>]
  let ``true`` () =
    True
    |> MsgPack.pack
    |> MsgPack.unpack
    |> should equal (Some True)

  [<Test>]
  let ``uint8`` () =
    check <| fun x ->
      let value = MUInt8 x
      Some value = (value |> MsgPack.pack |> MsgPack.unpack)

  [<Test>]
  let ``uint16`` () =
    check <| fun x ->
      let value = MUInt16 x
      Some value = (value |> MsgPack.pack |> MsgPack.unpack)

  [<Test>]
  let ``uint32`` () =
    check <| fun x ->
      let value = MUInt32 x
      Some value = (value |> MsgPack.pack |> MsgPack.unpack)

  [<Test>]
  let ``uint64`` () =
    check <| fun x ->
      let value = MUInt64 x
      Some value = (value |> MsgPack.pack |> MsgPack.unpack)

  [<Test>]
  let ``int8`` () =
    check <| fun x ->
      let value = MInt8 x
      Some value = (value |> MsgPack.pack |> MsgPack.unpack)

  [<Test>]
  let ``int16`` () =
    check <| fun x ->
      let value = MInt16 x
      Some value = (value |> MsgPack.pack |> MsgPack.unpack)

  [<Test>]
  let ``int32`` () =
    check <| fun x ->
      let value = MInt32 x
      Some value = (value |> MsgPack.pack |> MsgPack.unpack)

  [<Test>]
  let ``int64`` () =
    check <| fun x ->
      let value = MInt64 x
      Some value = (value |> MsgPack.pack |> MsgPack.unpack)

  [<Test>]
  let ``float32`` () =
    check <| fun x ->
      let value = MFloat32 x
      match value |> MsgPack.pack |> MsgPack.unpack with
      | Some (MFloat32 actual) when System.Single.IsNaN(actual) -> true
      | Some (MFloat32 actual) -> actual = x
      | _ -> false

  [<Test>]
  let ``float64`` () =
    check <| fun x ->
      let value = MFloat64 x
      match value |> MsgPack.pack |> MsgPack.unpack with
      | Some (MFloat64 actual) when System.Double.IsNaN(actual) -> true
      | Some (MFloat64 actual) -> actual = x
      | _ -> false

  [<Test>]
  let ``string`` () =
    check <| fun x ->
      (not <| System.String.IsNullOrEmpty(x)) ==> lazy
        (let value = MString x
        Some value = (value |> MsgPack.pack |> MsgPack.unpack))

  [<Test>]
  let ``binary`` () =
    check <| fun x ->
      let value = Binary x
      Some value = (value |> MsgPack.pack |> MsgPack.unpack)

  [<Test>]
  let ``array`` () =
    check <| fun xs ->
      let value =
        xs
        |> Array.map MInt32
        |> MArray
      Some value = (value |> MsgPack.pack |> MsgPack.unpack)

  [<Test>]
  let ``map`` () =
    check <| fun (xs: (int * int) list) ->
      let value =
        xs
        |> List.map (fun (k,v) -> (MInt32 k, MInt32 v))
        |> Map.ofList
        |> MMap
      Some value = (value |> MsgPack.pack |> MsgPack.unpack)

  type Person = {
    Name : string
    Age : int
  }
    with
      interface IPackable with
        member x.Code = 0x0cuy
        member x.Pack() =
          let name = x.Name |> MString |> MsgPack.pack
          let age = x.Age |> MInt32 |> MsgPack.pack
          [|
            yield HeadByte.Extended32
            yield! System.BitConverter.GetBytes(name.Length + age.Length + 1)
            yield MsgPack.Helper.getTypeCode x
            yield 0x92uy
            yield! name
            yield! age
          |]

  let parse' code body =
    match code with
    | 0x0cuy ->
      match MsgPack.unpack body with
      | Some (MArray [| MString name; MInt32 age |]) -> Parsed { Name = name; Age = age }
      | _ -> Raw(code, body)
    | _ -> Raw(code, body)

  [<Test>]
  let ``ext`` () =
    check <| fun name age ->
      (not <| System.String.IsNullOrEmpty(name)) ==> lazy
        (let person = Extended (Parsed { Name = name; Age = age })
        Some person = (person |> MsgPack.packExt |> MsgPack.unpackExt parse'))
