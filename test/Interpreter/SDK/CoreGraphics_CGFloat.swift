// RUN: %target-run-simple-swift | FileCheck %s

import CoreGraphics
import Foundation
import StdlibUnittest

var CGFloatTestCase = TestCase("CGFloat")

CGFloatTestCase.test("literals") {
  var flt: CGFloat = 4.125
  expectEqual(4.125, flt)

  flt = 42
  expectEqual(42, flt)
}

CGFloatTestCase.test("init") {
  expectEqual(0.0, CGFloat())
  expectEqual(4.125, CGFloat(Float(4.125)))
  expectEqual(4.125, CGFloat(Double(4.125)))

  expectEqual(42, CGFloat(Int(42)))
  expectEqual(42, CGFloat(Int8(42)))
  expectEqual(42, CGFloat(Int16(42)))
  expectEqual(42, CGFloat(Int32(42)))
  expectEqual(42, CGFloat(Int64(42)))
  expectEqual(42, CGFloat(UInt(42)))
  expectEqual(42, CGFloat(UInt8(42)))
  expectEqual(42, CGFloat(UInt16(42)))
  expectEqual(42, CGFloat(UInt32(42)))
  expectEqual(42, CGFloat(UInt64(42)))
}

CGFloatTestCase.test("initOtherTypesFromCGFloat") {
  let flt: CGFloat = 4.125

  expectEqual(4.125, Float(flt))
  expectEqual(4.125, Double(flt))

  expectEqual(4, Int(flt))
  expectEqual(4, Int8(flt))
  expectEqual(4, Int16(flt))
  expectEqual(4, Int32(flt))
  expectEqual(4, Int64(flt))
  expectEqual(4, UInt(flt))
  expectEqual(4, UInt8(flt))
  expectEqual(4, UInt16(flt))
  expectEqual(4, UInt32(flt))
  expectEqual(4, UInt64(flt))
}

CGFloatTestCase.test("comparisons") {
  let x = 3.14
  let y = 3.14
  let z = 2.71

  expectTrue(x == y)
  expectFalse(x != y)
  checkHashable(true, x, y)

  expectFalse(x == z)
  expectTrue(x != z)
  checkHashable(false, x, z)

  expectFalse(x < z)
  expectFalse(x <= z)
  expectTrue(x >= z)
  expectTrue(x > z)
  checkComparable(.GT, x, z)

  expectTrue(z < x)
  expectTrue(z <= x)
  expectFalse(z >= x)
  expectFalse(z > x)
  checkComparable(.LT, z, x)

  expectFalse(x < y)
  expectTrue(x <= y)
  expectTrue(x >= y)
  expectFalse(x > y)
  checkComparable(.EQ, x, y)
}

CGFloatTestCase.run()
// CHECK: {{^}}CGFloat: All tests passed


// CGFloat hashing
func hashing() {
  // CHECK-LABEL: hashing test
  println("hashing test")

  // CHECK-NEXT: hashValue = [[HASH_VALUE:[0-9]+]]
  let flt = CGFloat(2.71828)
  println("hashValue = \(flt.hashValue)")
  
}
hashing()

// Arithmetic
func arithmetic() {
  // CHECK-LABEL: arithmetic test
  println("arithmetic test")

  let x: CGFloat = 3.14
  let y: CGFloat = 2.71
  let z: CGFloat = 0.5

  // CHECK-NEXT: 5.85
  println(x + y)

  // CHECK-NEXT: 0.43
  println(x - y)

  // CHECK-NEXT: 8.5
  println(x * y)

  // CHECK-NEXT: 1.15
  println(x / y)

  // CHECK-NEXT: 0.14
  println(x % z)
}
arithmetic()

// Striding
func striding() {
  // CHECK-LABEL: striding test
  println("striding test")

  // CHECK-NEXT: 1.0
  // CHECK-NEXT: 1.5
  for f: CGFloat in stride(from: 1.0, to: 2.0, by: 0.5) {
    println(f)
  }

  // CHECK-NEXT: 1.0
  // CHECK-NEXT: 1.5
  // CHECK-NEXT: 2.0
  for f: CGFloat in stride(from: 1.0, through: 2.0, by: 0.5) {
    println(f)
  }
}
striding()

// Objective-C bridging to NSNumber.
func bridging() {
  // CHECK-LABEL: bridging test
  println("bridging test")

  var flt: CGFloat = 3.14159

  // CGFloat -> NSNumber conversion.
  // CHECK-NEXT: 3.14
  var num: NSNumber = flt
  println(num)

  // NSNumber -> CGFloat
  // CHECK-NEXT: 3.14
  flt = num
  println(flt)

  // Array bridging.
  var arr: [CGFloat] = [3.14159, 2.71818]
    
  // Array -> NSArray
  // CHECK-NEXT: (
  // CHECK-NEXT:   "3.14
  // CHECK-NEXT:   "2.71
  // CHECK-NEXT: )
  let nsarr: NSArray = arr
  println(nsarr)

  // CHECK-NEXT: [3.14{{[0-9]*}}, 2.71{{[0-9]*}}]
  arr = nsarr as [CGFloat]
  println(arr)
}
bridging()

