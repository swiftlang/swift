// RUN: %target-swift-emit-irgen %s -cxx-interoperability-mode=default -I %S/Inputs | %FileCheck %s

import UninstantiatableSpecialMembers

let nonCopyableValue = NonCopyableInst(123)
let nonCopyableDerived = DerivedNonCopyableInst(567)

// CHECK-NOT: scaryPoison
