// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default \
// RUN:     -verify-additional-file %S/Inputs/protected-dtor.h

import ProtectedDtor

let b = InheritMe() // expected-error {{cannot find 'InheritMe' in scope}}

let d = Derived()
let _ = d.inDerived

// FIXME: accessing fromBase should be fine, but doesn't work because we rely on
// importing the base class to access its members, but InheritMe is not
// importable beacuse it has a protected destructor.
let _ = d.fromBase // expected-error {{value of type 'Derived' has no member 'fromBase'}}
