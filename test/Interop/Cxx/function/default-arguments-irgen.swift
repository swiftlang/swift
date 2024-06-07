// RUN: %target-swift-emit-irgen -I %S/Inputs -cxx-interoperability-mode=swift-5.9 %s | %FileCheck %s
// RUN: %target-swift-emit-irgen -I %S/Inputs -cxx-interoperability-mode=swift-6 %s | %FileCheck %s
// RUN: %target-swift-emit-irgen -I %S/Inputs -cxx-interoperability-mode=upcoming-swift %s | %FileCheck %s

import DefaultArguments

// Make sure the default argument generator isn't emitted if it isn't used.
let _ = isZero(0)
// CHECK-NOT: __cxx__defaultArg_0__Z6isZeroi
