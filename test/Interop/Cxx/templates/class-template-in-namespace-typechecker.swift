// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -I %S/Inputs

import ClassTemplateInNamespace

let _ = Space.NestedNS1.ImplBox1(value: Space.NestedNS1.Impl())
let _ = Space.NestedNS2.ImplBox2(value: Space.NestedNS2.Impl())

let _ = Space.NestedNS1.ImplBox1(value: Space.NestedNS2.Impl()) // expected-error {{cannot convert value of type 'Space.NestedNS2.Impl' to expected argument type 'Space.NestedNS1.Impl'}}
let _ = Space.NestedNS2.ImplBox2(value: Space.NestedNS1.Impl()) // expected-error {{cannot convert value of type 'Space.NestedNS1.Impl' to expected argument type 'Space.NestedNS2.Impl'}}
