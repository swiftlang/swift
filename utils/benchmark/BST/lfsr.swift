// Linear function shift register.
//
// This is just to drive benchmarks. I don't make any claim about its
// strength. According to Wikipedia, it has the maximal period for a
// 32-bit register.
class LFSR {
  // Set the register to some seed that I pulled out of a hat.
  var lfsr = 0xb78978e7

  func shift() {
    lfsr = (lfsr >> 1) ^ (-(lfsr & 1) & 0xD0000001)
  }
  func randInt() -> Int {
    var result = 0
    for i in 0..<32 {
      result = (result << 1) | lfsr & 1
      shift()
    }
    return result
  }
}

func test() {
  var lfsr = LFSR()
  var rands = Dictionary<Int, Bool>()
  for i in 0..<1000 {
    let r = lfsr.randInt()
    assert(!rands[r])
    rands[r] = true
    print(r)
  }
}
