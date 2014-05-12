@asmname("mach_absolute_time") func __mach_absolute_time__() -> UInt64

struct RC4 {
  var State : UInt8[]
  var I : UInt8 = 0
  var J : UInt8 = 0

  init() {
    State = new UInt8[256]
  }

  mutating
  func initialize(inout Key: UInt8[]) {
    for i in 0...256 {
      State[i] = UInt8(i)
    }

    var j : UInt8 = 0
    for i in 0...256 {
      var K : UInt8 = Key[i % Key.count]
      var S : UInt8 = State[i]
      j = j &+ S &+ K
      swapByIndex(i, y: Int(j))
    }
  }

  mutating
  func swapByIndex(`x`: Int, y: Int) {
    let T1 : UInt8 = State[x]
    let T2 : UInt8 = State[y]
    State[x] = T2
    State[y] = T1
  }

  mutating
  func next() -> UInt8 {
    I = I &+ 1
    J = J &+ State[Int(I)]
    swapByIndex(Int(I), y: Int(J))
    return State[Int(State[Int(I)] &+ State[Int(J)]) & 0xFF]
  }

  mutating
  func encrypt(inout Data: UInt8[]) {
    var cnt = Data.count
    for i in 0...cnt {
      Data[i] = Data[i] ^ next()
    }
  }
}

func benchRC4_internal(messageLen : Int, iterations : Int) {
  var Secret = "This is my secret message"
  var Key    = "This is my key"
  var SecretData : UInt8[] = Secret.asUTF8()
  var KeyData    : UInt8[] = Key.asUTF8()

  var LongData : UInt8[] = new UInt8[messageLen]

  // Generate a long message.
  for i in 0...messageLen {
    LongData[i] = SecretData[i % SecretData.count]
  }

  var Enc = RC4()
  Enc.initialize(&KeyData)

  let start = __mach_absolute_time__()

  for i in 0...iterations {
    Enc.encrypt(&LongData)
  }

  let delta = __mach_absolute_time__() - start
  println("\(delta) nanoseconds. \(LongData[0])")
  println("\(Double(delta) / Double(iterations)) nanoseconds/lap")
}


func benchRC4() {
  benchRC4_internal(5000, 1000)
}

benchRC4()
