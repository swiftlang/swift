// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs -cxx-interoperability-mode=default -Xcc -std=c++23 -diagnostic-style llvm 2>&1 | %FileCheck %s

// TODO Linux CI stdlib doesn't support std::expected yet
// XFAIL: OS=linux-gnu

import StdExpected
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {} 

let nonCopExpected = NonCopyableExpected()
takeCopyable(nonCopExpected)
// CHECK: error: global function 'takeCopyable' requires that 'NonCopyableExpected' (aka {{.*}}) conform to 'Copyable'

let doe = DecoderOrError()
takeCopyable(doe)
// CHECK: error: global function 'takeCopyable' requires that 'DecoderOrError' (aka {{.*}}) conform to 'Copyable'

// CHECK-NOT: error
