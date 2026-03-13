// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -disable-availability-checking -I %S%{fs-sep}Inputs -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}frt-constructors.h

import FRTConstructors

let _ = FRTImplicitDefaultCtor1() // expected-warning {{cannot infer ownership of foreign reference value}}
let _ = FRTImplicitDefaultCtor0() // expected-warning {{cannot infer ownership of foreign reference value}}

let _ = FRTExplicitDefaultCtorNoAnnotation() // expected-warning {{cannot infer ownership of foreign reference value}}
let _ = FRTExplicitDefaultCtor1()
let _ = FRTExplicitDefaultCtor0()

let _ = FRTUserDefaultCtorNoAnnotation() // expected-warning {{cannot infer ownership of foreign reference value}}
let _ = FRTUserDefaultCtor1()
let _ = FRTUserDefaultCtor0()

let _ = FRTMixedConventionCtors()
let _ = FRTMixedConventionCtors(0)
let _ = FRTMixedConventionCtors(0, 0) // expected-warning {{cannot infer ownership of foreign reference value}}

let _ = FRTMixedConventionCtorsUnretainedByDefault()
let _ = FRTMixedConventionCtorsUnretainedByDefault(0)
let _ = FRTMixedConventionCtorsUnretainedByDefault(0, 0)
