// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s
// XFAIL: OS=linux-androideabi

import StdVector
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {} 
func takeCxxVector<V: CxxVector>(_ v: V) {} 

let vecNC = VectorOfNonCopyable()
takeCopyable(vecNC)
// CHECK: error: global function 'takeCopyable' requires that 'VectorOfNonCopyable' {{.*}} conform to 'Copyable'
// CHECK: note: 'where T: Copyable' is implicit here

takeCxxVector(vecNC) 
// CHECK: error: global function 'takeCxxVector' requires that 'VectorOfNonCopyable' {{.*}} conform to 'CxxVector'
// CHECK: note: where 'V' = 'VectorOfNonCopyable' {{.*}}

let vecPointer = VectorOfPointer()
takeCopyable(vecPointer)
takeCxxVector(vecPointer)
// CHECK-NOT: error
