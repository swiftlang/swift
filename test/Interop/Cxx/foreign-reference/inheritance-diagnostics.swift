// RUN: rm -rf %t
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs  %s -cxx-interoperability-mode=upcoming-swift -verify-additional-file %S/Inputs/inheritance.h -Xcc -Wno-return-type

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

let b1_ = returnB1_()
let b2_ = returnB2_()
let b3_ = returnB3_()
let d_ = returnD_()
// CHECK: warning: unable to infer 'SWIFT_SHARED_REFERENCE' for 'D_', although one of its C++ inheritance parents is marked as 'SWIFT_SHARED_REFERENCE'

let b1__ = returnB1__()
let b2__ = returnB2__()
// CHECK: error: multiple functions 'sameretain' found; there must be exactly one retain function for reference type 'B2__'
// CHECK: error: multiple functions 'samerelease' found; there must be exactly one release function for reference type 'B2__'
// CHECK: error: multiple functions 'sameretain' found; there must be exactly one retain function for reference type 'B1__'
// CHECK: error: multiple functions 'samerelease' found; there must be exactly one release function for reference type 'B1__'
let d__ = returnD__()
// CHECK: warning: unable to infer 'SWIFT_SHARED_REFERENCE' for 'D__', although one of its C++ inheritance parents is marked as 'SWIFT_SHARED_REFERENCE'
