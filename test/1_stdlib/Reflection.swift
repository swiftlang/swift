// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift -parse-stdlib %s -module-name Reflection -o %t/a.out
// RUN: %S/timeout.sh 360 %target-run %t/a.out | FileCheck %s
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
// CHECK-NEXT:      real: is this the real life?
// CHECK-NEXT:      imag: is it just fantasy?
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
// CHECK-NEXT:      y: 222
print("Root class:")
dump(Good())

// CHECK-LABEL: Subclass:
// CHECK-NEXT:    Reflection.Best #0
// CHECK-NEXT:      super: Reflection.Better
// CHECK-NEXT:        super: Reflection.Good
// CHECK-NEXT:          x: 11
// CHECK-NEXT:          y: 222
// CHECK-NEXT:        z: 333.5
// CHECK-NEXT:      w: 4444
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
// CHECK-NEXT:          y: 222
// CHECK-NEXT:        z: 333.5
// CHECK-NEXT:      w: 4444
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
// CHECK-NEXT: a
print("Character:")
print(_reflect(Character("a")).summary)

let range = 3...9
// CHECK-NEXT: 3..<10
print(_reflect(range).summary)
// CHECK-NEXT: startIndex=3
print("startIndex=\(_reflect(range)[0].1.summary)")

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
// CHECK-NEXT:          y: 222
// CHECK-NEXT:        z: 333.5
// CHECK-NEXT:      w: 4444
print("Barrable class:")
var barrable: Barrable = Best()
dump(barrable)
// CHECK-LABEL: second verse
// CHECK-NEXT:    Reflection.Best #0
print("second verse same as the first:")
dump(barrable)

// With _Reflectable protocols we extract the witness table from the container.
// CHECK-LABEL: _Reflectable int:
// CHECK-NEXT:   1
print("_Reflectable int:")
var reflectable: _Reflectable = 1
dump(reflectable)

// CHECK-NEXT: Logical: true
switch _reflect(true).quickLookObject {
  case .None: print("no quicklook")
  case .Some(let ql):
    switch ql {
      case .Logical(let x): print("Logical: \(x)")
      default: print("wrong quicklook type")
    }
}

// CHECK-NEXT: Hello world
print( _reflect(Optional<String>("Hello world")).summary )
// CHECK-NEXT: nil
print( _reflect(Optional<String>()).summary  )

let intArray = [1,2,3,4,5]
let intArrayMirror = _reflect(intArray)
// CHECK-NEXT: 5 elements
print(intArrayMirror.summary)
// CHECK-NEXT: [0]: 1
print("\(intArrayMirror[0].0): \(intArrayMirror[0].1.summary)")
// CHECK-NEXT: [4]: 5
print("\(intArrayMirror[4].0): \(intArrayMirror[4].1.summary)")

var justSomeFunction = { (x:Int) -> Int in return x + 1 }
// CHECK-NEXT: (Function)
print(_reflect(justSomeFunction).summary)

// CHECK-NEXT: Swift.String
print(_reflect(String.self).summary)

// CHECK-NEXT: CollectionOfOne(Howdy Swift!)
// CHECK-NEXT:  - element: Howdy Swift!
dump(CollectionOfOne("Howdy Swift!"))

// CHECK-NEXT: EmptyCollection
var emptyCollectionOfInt: EmptyCollection<Int> = EmptyCollection()
print(_reflect(emptyCollectionOfInt).summary)

// CHECK-NEXT: .One
print(_reflect(Bit.One).summary)

// CHECK-NEXT: ▿
// CHECK-NEXT: from: 1.0
// CHECK-NEXT: through: 12.15
// CHECK-NEXT: by: 3.14
dump(1.0.stride(through: 12.15, by: 3.14))

// CHECK-NEXT: UnsafeMutablePointer(nil)
var nilUnsafeMutablePointerString: UnsafeMutablePointer<String> = nil
print(_reflect(nilUnsafeMutablePointerString).summary)

// CHECK-NEXT: UnsafeMutablePointer(0x123456)
var randomUnsafeMutablePointerString = UnsafeMutablePointer<String>(
  bitPattern: 0x123456)
print(_reflect(randomUnsafeMutablePointerString).summary)

// CHECK-NEXT: Hello panda
var sanePointerString = UnsafeMutablePointer<String>.alloc(1)
sanePointerString.initialize("Hello panda")
print(_reflect(sanePointerString.memory).summary)
sanePointerString.destroy()
sanePointerString.dealloc(1)

// Don't crash on types with opaque metadata. rdar://problem/19791252
var rawPointer = unsafeBitCast(0 as Int, Builtin.RawPointer.self)
dump(rawPointer)
// CHECK: - (Opaque Value)

// CHECK-LABEL: and now our song is done
print("and now our song is done")

