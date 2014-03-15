module FsCheck.NUnit

open FsCheck
open NUnit.Framework

let runner =
  { new IRunner with
      member x.OnStartFixture t = ()
      member x.OnArguments(ntets: int, args: obj list, every: int -> obj list -> string) = ()
      member x.OnShrink(args, everyShrink) = ()
      member x.OnFinished(name, result) =
        match result with
        | TestResult.True _ -> ()
        | _ -> Assert.Fail(Runner.onFinishedToString name result) }

let config = { Config.Default with Runner = runner }

let check testable =
  Check.One ("", config, testable)

open System

// copy from https://github.com/fsharp/FsCheck/blob/1628773ed742fa223900fb706e44b20c3bd04db7/FsCheck/Arbitrary.fs
// TODO: upgrade FsCheck if new version of FsCheck is released.
type DefaultExtension =

  static member UInt16() =
    Arb.from<int>
    |> Arb.convert (abs >> uint16) int

  static member UInt32() =
    Arb.from<int>
    |> Arb.convert (abs >> uint32) int

  static member UInt64() =
    Arb.from<int>
    |> Arb.convert (abs >> uint64) int

  static member private fraction (a:int) (b:int) (c:int) = 
    double a + double b / (abs (double c) + 1.0)

  static member Float32() = 
    { new Arbitrary<float32>() with
      override x.Generator = 
        let fraction a b c = float32 (DefaultExtension.fraction a b c)
        Gen.frequency [
          (6, Gen.map3 fraction Arb.generate Arb.generate Arb.generate)
          (1, Gen.elements [ Single.NaN; Single.NegativeInfinity; Single.PositiveInfinity])
          (1, Gen.elements [ Single.MaxValue; Single.MinValue; Single.Epsilon])]
      override x.Shrinker fl =
        let (|<|) x y = abs x < abs y
        seq {
          if Single.IsInfinity fl || Single.IsNaN fl then 
            yield 0.0f
          else
            if fl < 0.0f then yield -fl
            let truncated = truncate fl
            if truncated |<| fl then yield truncated }
        |> Seq.distinct
    }

do Arb.register<DefaultExtension>() |> ignore
