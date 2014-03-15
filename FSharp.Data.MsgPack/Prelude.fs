namespace FSharp.Data

[<AutoOpen>]
module internal Prelude =

  let uint16ToBytes (x: uint16) = System.BitConverter.GetBytes(x)
  let uint32ToBytes (x: uint32) = System.BitConverter.GetBytes(x)
  let uint64ToBytes (x: uint64) = System.BitConverter.GetBytes(x)
  let int16ToBytes (x: int16) = System.BitConverter.GetBytes(x)
  let intToBytes (x: int) = System.BitConverter.GetBytes(x)
  let int64ToBytes (x: int64) = System.BitConverter.GetBytes(x)
  let float32ToBytes (x: float32) = System.BitConverter.GetBytes(x)
  let floatToBytes (x: float) = System.BitConverter.GetBytes(x)
  let stringToBytes (s: string) = System.Text.Encoding.UTF8.GetBytes(s)
