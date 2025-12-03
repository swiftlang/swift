// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s

import StdStack
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {} 

let stackNonCopyable = StackOfNonCopyable() 
takeCopyable(stackNonCopyable) 
// CHECK: error: global function 'takeCopyable' requires that 'StackOfNonCopyable' {{.*}} conform to 'Copyable'
// CHECK: note: 'where T: Copyable' is implicit here
