// UNSUPPORTED: linux
// <rdar://problem/42793848>
// RUN: %target-swift-remoteast-test %s | %FileCheck %s

// REQUIRES: swift-remoteast-test

@_silgen_name("printDynamicTypeAndAddressForExistential")
func printDynamicTypeAndAddressForExistential<T>(_: T)

struct MyStruct<T, U, V> {
  let x: T
  let y: U
  let z: V
}

// Case one, small opaque (fits into the inline buffer).
// CHECK: MyStruct<Int, Int, Int>
let smallStruct = MyStruct(x : 1, y: 2, z: 3)
printDynamicTypeAndAddressForExistential(smallStruct as Any)

// Case two, large opaque (boxed representation).
// CHECK-NEXT: MyStruct<(Int, Int, Int), (Int, Int, Int), (Int, Int, Int)>
let largeStruct = MyStruct(x: (1,1,1), y: (2,2,2), z: (3,3,3))
printDynamicTypeAndAddressForExistential(largeStruct as Any)

class MyClass<T, U> {
  let x: T
  let y: (T, U)
  init(x: T, y: (T, U)) {
    self.x = x
    self.y = y
  }
}

// Case three, class existential (adheres to AnyObject protocol).a
// CHECK-NEXT: MyClass<Int, Int>
let mc = MyClass(x : 23, y : (42, 44)) as AnyObject
printDynamicTypeAndAddressForExistential(mc)

enum MyError : Error {
    case a
    case b
}

// Case four: error existential.
// CHECK-NEXT: MyError
let q : Error  = MyError.a
printDynamicTypeAndAddressForExistential(q)

// Case five: existential metatypes.
// CHECK-NEXT: Any.Type
let metatype : Any.Type = Any.self
printDynamicTypeAndAddressForExistential(metatype)
