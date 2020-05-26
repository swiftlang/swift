// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/ExternPrecedences.swift

// RUN: %target-typecheck-verify-swift -I%t

import ExternPrecedences

precedencegroup A {
  higherThan: A // expected-error {{cycle in 'higherThan' relation}}
}

precedencegroup B { // expected-note {{precedence group declared here}}
  lowerThan: B // expected-error {{cycle in 'lowerThan' relation}}
  // expected-error@-1{{precedence group cannot be given lower precedence than group in same module; make the other precedence group higher than this one instead}}
}

precedencegroup C0 {
  higherThan: C1 // expected-error {{cycle in 'higherThan' relation}}
}
precedencegroup C1 {
  higherThan: C0 // expected-note{{through reference to precedence group 'C0' here}}
}

precedencegroup D0 {
  higherThan: D1 // expected-error {{cycle in 'higherThan' relation}}
}
precedencegroup D1 {
  higherThan: D2 // expected-note{{through reference to precedence group 'D2' here}}
}
precedencegroup D2 {
  higherThan: D0 // expected-note{{through reference to precedence group 'D0' here}}
}

precedencegroup E0 {
  higherThan: Extern1 // expected-error{{cycle in higherThan relation: E0 -> Extern1 -> Extern0 -> E0}}
  lowerThan: Extern0
}
