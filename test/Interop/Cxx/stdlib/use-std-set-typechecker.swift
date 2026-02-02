// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s

import StdSet
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {} 

let setNonCopyable = SetOfNonCopyable() 
takeCopyable(setNonCopyable) 
// CHECK: error: global function 'takeCopyable' requires that 'SetOfNonCopyable' {{.*}} conform to 'Copyable'
// CHECK: note: 'where T: Copyable' is implicit here
