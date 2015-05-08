@asmname("mach_absolute_time") func __mach_absolute_time__() -> UInt64

struct RC4 {
  var State : UInt8[]
  var I : UInt8 = 0
  var J : UInt8 = 0

  init() {
    State = new UInt8[256]
  }

  mutating
  func initialize(Key: UInt8[]) {
    for (var i = 0; i < 256; i++) {
      State[i] = UInt8(i)
    }

    var j : UInt8 = 0
    for (var i = 0; i < 256; i++) {
      var K : UInt8 = Key[i % Key.count]
      var S : UInt8 = State[i]
      j = j &+ S &+ K
      swapByIndex(i, y: Int(j))
    }
  }

  mutating
  func swapByIndex(x: Int, y: Int) {
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
    for (var i = 0; i < cnt; i++) {
      Data[i] = Data[i] ^ next()
    }
  }
}

func benchRC4_internal(messageLen : Int, iterations : Int) {
  let Secret = "This is my secret message"
  let Key    = "This is my key"
  let SecretData : UInt8[] = Array(Secret.utf8)
  let KeyData    : UInt8[] = Array(Key.utf8)

  var LongData : UInt8[] = new UInt8[messageLen]

  // Generate a long message.
  for (var i = 0; i < messageLen; i++) {
    LongData[i] = SecretData[i % SecretData.count]
  }

  var Enc = RC4()
  Enc.initialize(KeyData)

  let start = __mach_absolute_time__()

  for (var i = 0; i < iterations; i++) {
    Enc.encrypt(&LongData)
  }

  let delta = __mach_absolute_time__() - start
  print("\(delta) nanoseconds. \(LongData[0])")
  print("\(Double(delta) / Double(iterations)) nanoseconds/lap")
}


func benchRC4() {
  benchRC4_internal(5000, 100000)
}

benchRC4()
