// RUN: %target-run-simple-swift | FileCheck %s

import CoreGraphics
import Foundation

// CGFloat initialization
func initialization() {
  // CHECK-LABEL: initialization test
  println("initialization test")

  // CHECK-NEXT: 0.0
  var flt = CGFloat()
  println(flt)

  // CHECK-NEXT: 3.14
  let f: Float = 3.14159
  flt = CGFloat(f)
  println(flt)

  // CHECK-NEXT: 3.14
  let d: Double = 3.14159
  flt = CGFloat(d)
  println(flt)

  // CHECK-NEXT: 3
  var i: Int = 3
  flt = CGFloat(i)
  println(flt)

  // CHECK-NEXT: 24
  let i8: Int8 = 24
  flt = CGFloat(i8)
  println(flt)
}
initialization()

// CGFloat hashing
func hashing() {
  // CHECK-LABEL: hashing test
  println("hashing test")

  // CHECK-NEXT: hashValue = [[HASH_VALUE:[0-9]+]]
  let flt = CGFloat(2.71828)
  println("hashValue = \(flt.hashValue)")
  
}
hashing()

// CGFloat literal conversion
func literals() {
  // CHECK-LABEL: literals test
  println("literals test")

  // CHECK-NEXT: 3.14
  var flt: CGFloat = 3.14159
  println(flt)

  // CHECK-NEXT: 3
  flt = 3
  println(flt)
}
literals()

// Initialization of other numeric types from CGFloat
func otherInitializations() {
  // CHECK-LABEL: initialization of other numeric types test
  println("initialization of other numeric types test")
  
  let flt: CGFloat = 3.14159

  // CHECK-NEXT: 3
  let i = Int(flt)
  println(i)

  // CHECK-NEXT: 3.14
  let f = Float(flt)
  println(f)

  // CHECK-NEXT: 3.14
  let d = Double(flt)
  println(d)  

  // CHECK-NEXT: 3
  let u8 = UInt8(flt)
  println(u8)

  // CHECK-NEXT: 3
  let ui = UInt(flt)
  println(ui)
}
otherInitializations()

// Comparisons
func comparisons() {
  // CHECK-LABEL: comparisons test
  println("comparisons test")

  let x = 3.14
  let y = 3.14
  let z = 2.71

  // Equality
  assert(x == y)
  assert(!(x == z))
  assert(x != z)
  assert(!(x != y))

  assert(z < x)
  assert(!(x < z))
  assert(z <= x)
  assert(x <= x)
  assert(!(x <= z))
  assert(x > z)
  assert(!(z > x))
  assert(x >= z)
  assert(x >= x)
  assert(!(z >= x))
}
comparisons()

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

