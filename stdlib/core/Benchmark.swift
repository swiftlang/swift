////////////////////////////////////////////////////////////////////////////////
//
// This file contains a small number of Swift benchmarks.
// At the moment we can't serialize large chunks of the swift standard library,
// which prevents us from optimizing user code. In order to evaluate the
// performance of Swift we decided to place some benchmarks in the standard
// library and evaluate the optimized versions.
//
// This file will be removed as soon as rdar://14747929 is fixed.
//
////////////////////////////////////////////////////////////////////////////////
class RC4 {
  var State : UInt8[]
  var I : UInt8 = 0
  var J : UInt8 = 0

  init() {
    State = new UInt8[256]
  }

  func initialize(Key: UInt8[]) {
    for i in 0..256 {
      State[i] = UInt8(i)
    }

    var j : UInt8 = 0
    for i in 0..256 {
      var K : UInt8 = Key[i % Key.count]
      var S : UInt8 = State[i]
      j = j &+ S &+ K
      swapByIndex(i, Int(j))
    }
  }

  func swapByIndex(x: Int, y: Int) {
    var T1 : UInt8 = State[x]
    var T2 : UInt8 = State[y]
    State[x] = T2
    State[y] = T1
  }

  func next() -> UInt8 {
    I = I &+ 1
    J = J &+ State[Int(I)]
    swapByIndex(Int(I), Int(J))
    return State[Int(State[Int(I)] &+ State[Int(J)]) & 0xFF]
  }

  func encrypt(Data: UInt8[]) {
    var cnt = Data.count
    for i in 0..cnt {
      Data[i] = Data[i] ^ next()
    }
  }
}

func benchRC4(messageLen : Int, iterations : Int, validate : Bool) {
  var Secret = "This is my secret message"
  var Key    = "This is my key"
  var SecretData : UInt8[] = Secret.asUInt8()
  var KeyData    : UInt8[] = Key.asUInt8()

  var LongData : UInt8[] = new UInt8[messageLen]

  println("Generating data ... ")

  // Generate a long message. 
  for i in 0..messageLen {
    LongData[i] = SecretData[i % SecretData.count]
  }

  // Generate the Encryptor and Decryptor classes.
  // FIXME: Passing KeyData to the c'tor does not type check.
  var Enc = RC4()
  var Dec = RC4()
  Enc.initialize(KeyData)
  Dec.initialize(KeyData)

  println("Starting benchmark...")
    for i in 0..iterations {
      Enc.encrypt(LongData)
      Dec.encrypt(LongData)
    }

  if (validate) {
    println("Validating ...")
    for i in 0..messageLen {
      if (LongData[i] != SecretData[i % SecretData.count]) {
        println("Error at \(i)");
      }
    }
  }

  println("Done.")
}

