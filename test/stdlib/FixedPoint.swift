// RUN: %target-run-simple-swift | FileCheck %s

func assertEq<T1 : Equatable, T2 : Equatable>(a : (T1, T2), b : (T1, T2)) {
  assert(a.0 == b.0)
  assert(a.1 == b.1)
}

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


func testOverflowCheckOperations() {
  assertEq(Int8.addWithOverflow(4, 5), (9, false))
  assertEq(Int8.addWithOverflow(1, 127), (-128, true))
  assertEq(UInt8.multiplyWithOverflow(2,128), (0, true))
  println("testOverflowCheckOperations done")
}

testOverflowCheckOperations()
// CHECK-NEXT: testOverflowCheckOperations done

let x: UInt32 = 0xdeadbeef
println("0x\(String(x.bigEndian, radix: 16))") // CHECK-NEXT: 0xefbeadde

let y = UInt32(bigEndian: 0xdeadbeef)
println("0x\(String(y.bigEndian, radix: 16))") // CHECK-NEXT: 0xdeadbeef

println(Int64(4).byteSwapped)                  // CHECK-NEXT: 288230376151711744
