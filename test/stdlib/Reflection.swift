// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-stdlib %s -module-name Reflection -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %S/timeout.sh 360 %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test
// FIXME: timeout wrapper is necessary because the ASan test runs for hours

//
// DO NOT add more tests to this file.  Add them to test/1_stdlib/Runtime.swift.
//

import Swift

// A more interesting struct type.
struct Complex<T> {
  let real, imag: T
}
// CHECK-LABEL: Complex:
print("Complex:")
// CHECK-NEXT:    Reflection.Complex<Swift.Double>
// CHECK-NEXT:      real: 1.5
// CHECK-NEXT:      imag: 0.75
dump(Complex<Double>(real: 1.5, imag: 0.75))
// CHECK-NEXT:    Reflection.Complex<Swift.Double>
// CHECK-NEXT:      real: -1.5
// CHECK-NEXT:      imag: -0.75
dump(Complex<Double>(real: -1.5, imag: -0.75))
// CHECK-NEXT:    Reflection.Complex<Swift.Int>
// CHECK-NEXT:      real: 22
// CHECK-NEXT:      imag: 44
dump(Complex<Int>(real: 22, imag: 44))
// CHECK-NEXT:    Reflection.Complex<Swift.String>
// CHECK-NEXT:      real: "is this the real life?"
// CHECK-NEXT:      imag: "is it just fantasy?"
dump(Complex<String>(real: "is this the real life?", 
                     imag: "is it just fantasy?"))


// Test destructuring of a pure Swift class hierarchy.
class Good {
  let x: UInt = 11
  let y: String = "222"
}

class Better : Good {
  let z: Double = 333.5
}

class Best : Better {
  let w: String = "4444"
}

// CHECK-LABEL: Root class:
// CHECK-NEXT:    Reflection.Good #0
// CHECK-NEXT:      x: 11
// CHECK-NEXT:      y: "222"
print("Root class:")
dump(Good())

// CHECK-LABEL: Subclass:
// CHECK-NEXT:    Reflection.Best #0
// CHECK-NEXT:      super: Reflection.Better
// CHECK-NEXT:        super: Reflection.Good
// CHECK-NEXT:          x: 11
// CHECK-NEXT:          y: "222"
// CHECK-NEXT:        z: 333.5
// CHECK-NEXT:      w: "4444"
print("Subclass:")
dump(Best())

// Test protocol types, which reflect as their dynamic types.
// CHECK-LABEL: Any int:
// CHECK-NEXT:    1
print("Any int:")
var any: Any = 1
dump(any)

// CHECK-LABEL: Any class:
// CHECK-NEXT:    Reflection.Best #0
// CHECK-NEXT:      super: Reflection.Better
// CHECK-NEXT:        super: Reflection.Good
// CHECK-NEXT:          x: 11
// CHECK-NEXT:          y: "222"
// CHECK-NEXT:        z: 333.5
// CHECK-NEXT:      w: "4444"
print("Any class:")
any = Best()
dump(any)
// CHECK-LABEL: second verse
// CHECK-NEXT:    Reflection.Best #0
print("second verse same as the first:")
dump(any)

// CHECK-LABEL: Any double:
// CHECK-NEXT:    2.5
print("Any double:")
any = 2.5
dump(any)

// CHECK-LABEL: Character:
// CHECK-NEXT:   "a"
print("Character:")
dump(Character("a"))

protocol Fooable {}
extension Int : Fooable {}
extension Double : Fooable {}

// CHECK-LABEL: Fooable int:
// CHECK-NEXT:    1
print("Fooable int:")
var fooable: Fooable = 1
dump(fooable)

// CHECK-LABEL: Fooable double:
// CHECK-NEXT:    2.5
print("Fooable double:")
fooable = 2.5
dump(fooable)

protocol Barrable : class {}
extension Best: Barrable {}

// CHECK-LABEL: Barrable class:
// CHECK-NEXT:    Reflection.Best #0
// CHECK-NEXT:      super: Reflection.Better
// CHECK-NEXT:        super: Reflection.Good
// CHECK-NEXT:          x: 11
// CHECK-NEXT:          y: "222"
// CHECK-NEXT:        z: 333.5
// CHECK-NEXT:      w: "4444"
print("Barrable class:")
var barrable: Barrable = Best()
dump(barrable)
// CHECK-LABEL: second verse
// CHECK-NEXT:    Reflection.Best #0
// CHECK-NEXT:      super: Reflection.Better
// CHECK-NEXT:        super: Reflection.Good
// CHECK-NEXT:          x: 11
// CHECK-NEXT:          y: "222"
// CHECK-NEXT:        z: 333.5
// CHECK-NEXT:      w: "4444"
print("second verse same as the first:")
dump(barrable)

// CHECK-NEXT: Logical: true
switch true.customPlaygroundQuickLook {
  case .bool(let x): print("Logical: \(x)")
  default: print("wrong quicklook type")
}

let intArray = [1,2,3,4,5]
// CHECK-NEXT: 5 elements
// CHECK-NEXT: 1
// CHECK-NEXT: 2
// CHECK-NEXT: 3
// CHECK-NEXT: 4
// CHECK-NEXT: 5
dump(intArray)

var justSomeFunction = { (x:Int) -> Int in return x + 1 }
// CHECK-NEXT: (Function)
dump(justSomeFunction as Any)

// CHECK-NEXT: Swift.String
dump(String.self)

// CHECK-NEXT: ▿
// CHECK-NEXT: from: 1.0
// CHECK-NEXT: through: 12.15
// CHECK-NEXT: by: 3.14
dump(stride(from: 1.0, through: 12.15, by: 3.14))

// CHECK-NEXT: nil
var nilUnsafeMutablePointerString: UnsafeMutablePointer<String>?
dump(nilUnsafeMutablePointerString)

// CHECK-NEXT: 123456
// CHECK-NEXT: - pointerValue: 1193046
var randomUnsafeMutablePointerString = UnsafeMutablePointer<String>(
  bitPattern: 0x123456)!
dump(randomUnsafeMutablePointerString)

// CHECK-NEXT: Hello panda
var sanePointerString = UnsafeMutablePointer<String>.allocate(capacity: 1)
sanePointerString.initialize(to: "Hello panda")
dump(sanePointerString.pointee)
sanePointerString.deinitialize(count: 1)
sanePointerString.deallocate()

// Don't crash on types with opaque metadata. rdar://problem/19791252
// CHECK-NEXT: (Opaque Value)
var rawPointer = unsafeBitCast(0 as Int, to: Builtin.RawPointer.self)
dump(rawPointer)

// CHECK-LABEL: and now our song is done
print("and now our song is done")
