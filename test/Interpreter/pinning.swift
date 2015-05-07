// RUN: rm -rf %t  &&  mkdir -p %t
// RUN: %target-build-swift -parse-stdlib %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

import Swift
import SwiftShims

typealias _HeapObject = SwiftShims.HeapObject

class C {
  var value: AnyObject? = nil
  deinit { println("deallocated") }
}


println("starting")
var ptr = Builtin.castToNativeObject(C())

//CHECK:  1. true
println(" 1. \(_getBool(Builtin.isUnique(&ptr))))")
//CHECK:  2. true
println(" 2. \(_getBool(Builtin.isUniqueOrPinned(&ptr))))")

var ptr2 : Builtin.NativeObject? = ptr

//CHECK:  3. false
println(" 3. \(_getBool(Builtin.isUnique(&ptr))))")
//CHECK:  4. false
println(" 4. \(_getBool(Builtin.isUniqueOrPinned(&ptr))))")

ptr2 = nil

//CHECK:  5. true
println(" 5. \(_getBool(Builtin.isUnique(&ptr)))")
//CHECK:  6. true
println(" 6. \(_getBool(Builtin.isUniqueOrPinned(&ptr)))")

ptr2 = ptr

//CHECK:  7. false
println(" 7. \(_getBool(Builtin.isUnique(&ptr)))")
//CHECK:  8. false
println(" 8. \(_getBool(Builtin.isUniqueOrPinned(&ptr)))")

let token : Builtin.NativeObject? = Builtin.tryPin(ptr)

//CHECK:  9. false
println(" 9. \(_getBool(Builtin.isUnique(&ptr)))")
//CHECK: 10. true
println("10. \(_getBool(Builtin.isUniqueOrPinned(&ptr)))")

Builtin.unpin(token)

//CHECK: 11. false
println("11. \(_getBool(Builtin.isUnique(&ptr)))")
//CHECK: 12. false
println("12. \(_getBool(Builtin.isUniqueOrPinned(&ptr)))")

ptr2 = ptr

//CHECK: 13. false
println("13. \(_getBool(Builtin.isUnique(&ptr)))")
//CHECK: 14. false
println("14. \(_getBool(Builtin.isUniqueOrPinned(&ptr)))")

