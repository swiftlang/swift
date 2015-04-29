// RUN: rm -rf %t  &&  mkdir -p %t
// RUN: %target-build-swift -parse-stdlib %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

import Swift
import SwiftShims

typealias _HeapObject = SwiftShims.HeapObject
func isUniquelyReferenced(inout x: Builtin.NativeObject) -> Bool {
  let p: UnsafePointer<_HeapObject> = Builtin.reinterpretCast(x)
  return _swift_isUniquelyReferenced_nonNull_native(p)
}

func isUniquelyReferencedOrPinned(inout x: Builtin.NativeObject) -> Bool {
  let p: UnsafePointer<_HeapObject> = Builtin.reinterpretCast(x)
  return _swift_isUniquelyReferencedOrPinned_nonNull_native(p)
}

class C {
  var value: AnyObject? = nil
  deinit { println("deallocated") }
}


println("starting")
var ptr = Builtin.castToNativeObject(C())

//CHECK:  1. true
println(" 1. \(isUniquelyReferenced(&ptr))")
//CHECK:  2. true
println(" 2. \(isUniquelyReferencedOrPinned(&ptr))")

var ptr2 : Builtin.NativeObject? = ptr

//CHECK:  3. false
println(" 3. \(isUniquelyReferenced(&ptr))")
//CHECK:  4. false
println(" 4. \(isUniquelyReferencedOrPinned(&ptr))")

ptr2 = nil

//CHECK:  5. true
println(" 5. \(isUniquelyReferenced(&ptr))")
//CHECK:  6. true
println(" 6. \(isUniquelyReferencedOrPinned(&ptr))")

ptr2 = ptr

//CHECK:  7. false
println(" 7. \(isUniquelyReferenced(&ptr))")
//CHECK:  8. false
println(" 8. \(isUniquelyReferencedOrPinned(&ptr))")

let token : Builtin.NativeObject? = Builtin.tryPin(ptr)

//CHECK:  9. false
println(" 9. \(isUniquelyReferenced(&ptr))")
//CHECK: 10. true
println("10. \(isUniquelyReferencedOrPinned(&ptr))")

Builtin.unpin(token)

//CHECK: 11. false
println("11. \(isUniquelyReferenced(&ptr))")
//CHECK: 12. false
println("12. \(isUniquelyReferencedOrPinned(&ptr))")

ptr2 = ptr

//CHECK: 13. false
println("13. \(isUniquelyReferenced(&ptr))")
//CHECK: 14. false
println("14. \(isUniquelyReferencedOrPinned(&ptr))")

