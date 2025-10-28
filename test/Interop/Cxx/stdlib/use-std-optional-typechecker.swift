// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s

import StdOptional
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {} 

let nonNilOptNonCopyable = getNonNilOptionalHasDeletedCopyCtor()
takeCopyable(nonNilOptNonCopyable)
// CHECK: error: global function 'takeCopyable' requires that 'StdOptionalHasDeletedCopyCtor' {{.*}} conform to 'Copyable'
// CHECK: note: 'where T: Copyable' is implicit here
