// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s

import StdSet
import CxxStdlib
import Cxx

func takeCopyable<T: Copyable>(_ x: T) {} 
func takeCxxSet<S: CxxSet>(_ s: S) {}
func takeCxxDefaultConstructibleSet<S: CxxDefaultConstructibleSet>(_ s: S) {}

let setNonCopyable = SetOfNonCopyable() 
takeCopyable(setNonCopyable) 
// CHECK: error: global function 'takeCopyable' requires that 'SetOfNonCopyable' {{.*}} conform to 'Copyable'
// CHECK: note: 'where T: Copyable' is implicit here

takeCxxDefaultConstructibleSet(SetOfCInt())
let setWithCustomComparator = initSetOfCIntWithCustomComparator()
takeCxxSet(setWithCustomComparator)
// CHECK-NOT: error:
takeCxxDefaultConstructibleSet(setWithCustomComparator)
// CHECK: error: global function 'takeCxxDefaultConstructibleSet' requires that 'SetOfCIntWithCustomComparator' {{.*}} conform to 'CxxDefaultConstructibleSet'
