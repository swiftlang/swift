// RUN: %target-run-simple-swift | FileCheck %s
struct MyInt32 : BitwiseOperationsType {
  var underlying: Int32

  static var allZeros: MyInt32 { return MyInt32(underlying: 0) }
}

func & (lhs: MyInt32, rhs: MyInt32) -> MyInt32 {
  return MyInt32(underlying: lhs.underlying & rhs.underlying)
}

func |(lhs: MyInt32, rhs: MyInt32) -> MyInt32 {
  return MyInt32(underlying: lhs.underlying | rhs.underlying)
}

func ^(lhs: MyInt32, rhs: MyInt32) -> MyInt32 {
  return MyInt32(underlying: lhs.underlying ^ rhs.underlying)
}

prefix func ~(x: MyInt32) -> MyInt32 {
  return MyInt32(underlying: ~x.underlying)
}

// |=
var a = MyInt32(underlying: 0x3)
a |= MyInt32(underlying: 0x4)
assert(a.underlying == 0x7)

// &=
a &= MyInt32(underlying: 0x5)
assert(a.underlying == 0x5)

// ^= 
a ^= MyInt32(underlying: 0x6)
assert(a.underlying == 0x3)

// CHECK: done
print("done")
