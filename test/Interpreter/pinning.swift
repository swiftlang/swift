// RUN: rm -rf %t  &&  mkdir -p %t
// RUN: %target-build-swift -parse-stdlib %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

import Swift
import SwiftShims

typealias _HeapObject = SwiftShims.HeapObject

class C {
  var value: AnyObject? = nil
  deinit { print("deallocated") }
}


print("starting")
var ptr = Builtin.castToNativeObject(C())

//CHECK:  1. true
print(" 1. \(_getBool(Builtin.isUnique(&ptr))))")
//CHECK:  2. true
print(" 2. \(_getBool(Builtin.isUniqueOrPinned(&ptr))))")

var ptr2 : Builtin.NativeObject? = ptr

//CHECK:  3. false
print(" 3. \(_getBool(Builtin.isUnique(&ptr))))")
//CHECK:  4. false
print(" 4. \(_getBool(Builtin.isUniqueOrPinned(&ptr))))")

ptr2 = nil

//CHECK:  5. true
print(" 5. \(_getBool(Builtin.isUnique(&ptr)))")
//CHECK:  6. true
print(" 6. \(_getBool(Builtin.isUniqueOrPinned(&ptr)))")

ptr2 = ptr

//CHECK:  7. false
print(" 7. \(_getBool(Builtin.isUnique(&ptr)))")
//CHECK:  8. false
print(" 8. \(_getBool(Builtin.isUniqueOrPinned(&ptr)))")

let token : Builtin.NativeObject? = Builtin.tryPin(ptr)

//CHECK:  9. false
print(" 9. \(_getBool(Builtin.isUnique(&ptr)))")
//CHECK: 10. true
print("10. \(_getBool(Builtin.isUniqueOrPinned(&ptr)))")

Builtin.unpin(token)

//CHECK: 11. false
print("11. \(_getBool(Builtin.isUnique(&ptr)))")
//CHECK: 12. false
print("12. \(_getBool(Builtin.isUniqueOrPinned(&ptr)))")

ptr2 = ptr

//CHECK: 13. false
print("13. \(_getBool(Builtin.isUnique(&ptr)))")
//CHECK: 14. false
print("14. \(_getBool(Builtin.isUniqueOrPinned(&ptr)))")

