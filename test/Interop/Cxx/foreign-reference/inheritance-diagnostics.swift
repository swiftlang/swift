// RUN: %target-swift-frontend -typecheck -I %S/Inputs  %s -cxx-interoperability-mode=default 2>&1 | %FileCheck %s

import Inheritance

let baseRef1 = returnBaseRef1()
let baseRef2 = returnBaseRef2()
let derivedFromBaseRef1AndBaseRef2 = returnDerivedFromBaseRef1AndBaseRef2()
// CHECK: warning: unable to infer 'SWIFT_SHARED_REFERENCE' for 'DerivedFromBaseRef1AndBaseRef2', although one of its C++ inheritance parents is marked as 'SWIFT_SHARED_REFERENCE'

let derivedFromBaseRef3 = returnDerivedFromBaseRef3()
// CHECK-NOT: warning: unable to infer 'SWIFT_SHARED_REFERENCE' for 'DerivedFromBaseRef3', although one of its C++ inheritance parents is marked as 'SWIFT_SHARED_REFERENCE'

let b1 = returnB1()
let b2 = returnB2()
let b3 = returnB3()
let d = returnD()
// CHECK: warning: unable to infer 'SWIFT_SHARED_REFERENCE' for 'D', although one of its C++ inheritance parents is marked as 'SWIFT_SHARED_REFERENCE'
