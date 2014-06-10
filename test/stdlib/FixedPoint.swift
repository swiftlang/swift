// RUN: %target-run-simple-swift | FileCheck %s

func testBitwiseOperationsImpl<T : UnsignedInteger>(_: T.Type) {
  var x = numericCast(0b11_1010_00) as T
  var y = numericCast(0b10_1100_10) as T

  assert(x & y == 0b10_1000_00)
  assert(x | y == 0b11_1110_10)
  assert(x ^ y == 0b01_0110_10)
  assert((~x) & 0xff == 0b00_0101_11)

  var z = T.allZeros
  assert(x | z == x)
  assert(x ^ z == x)
  assert(x & z == z)
  assert(x & ~z == x)
}

func testBitwiseOperations() {
  testBitwiseOperationsImpl(UInt8.self)
  testBitwiseOperationsImpl(UInt16.self)
  testBitwiseOperationsImpl(UInt32.self)
  testBitwiseOperationsImpl(UInt64.self)

  println("testBitwiseOperations done")
}
testBitwiseOperations()
// CHECK: testBitwiseOperations done

let bigEndian: UInt32 = 0xdeadbeef
println("0x\(String(bigEndian.byteSwapped, radix: 16))") // CHECK: 0xefbeadde
