namespace FSharp.Data.MsgPack.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open FSharp.Data
open MsgPackValue

[<TestFixture>]
module OldSpecTest =

  [<Test>]
  let ``string`` () =
    check <| fun x ->
      let value = MString x
      let raw = Binary (System.Text.Encoding.UTF8.GetBytes(x))
      Some raw = (value |> MsgPack.OldSpec.pack |> MsgPack.OldSpec.unpack)

  [<Test>]
  let ``binary`` () =
    check <| fun x ->
      let value = Binary x
      Some value = (value |> MsgPack.OldSpec.pack |> MsgPack.OldSpec.unpack)
