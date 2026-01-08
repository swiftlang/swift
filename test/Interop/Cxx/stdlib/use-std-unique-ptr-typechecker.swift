// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

import StdUniquePtr
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {} 

let vecUniquePtr = getVectorNonCopyableUniquePtr()
takeCopyable(vecUniquePtr)
// CHECK: error: global function 'takeCopyable' requires that 'std{{.*}}vector{{.*}}unique_ptr{{.*}}NonCopyable{{.*}}' conform to 'Copyable'
