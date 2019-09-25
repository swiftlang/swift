// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/ExternPrecedences.swift

// RUN: %target-typecheck-verify-swift -I%t

import ExternPrecedences

precedencegroup A {
  higherThan: A // expected-error {{cycle in higherThan relation: A -> A}}
}

precedencegroup B { // expected-note {{precedence group declared here}}
  lowerThan: B // expected-error {{precedence group cannot be given lower precedence than group in same module; make the other precedence group higher than this one instead}}
}

precedencegroup C0 {
  higherThan: C1 // expected-error {{cycle in higherThan relation: C0 -> C1 -> C0}}
}
precedencegroup C1 {
  higherThan: C0
}

precedencegroup D0 {
  higherThan: D1 // expected-error {{cycle in higherThan relation: D0 -> D1 -> D2 -> D0}}
}
precedencegroup D1 {
  higherThan: D2
}
precedencegroup D2 {
  higherThan: D0
}

precedencegroup E0 {
  higherThan: Extern1 // expected-error {{cycle in higherThan relation: E0 -> Extern1 -> Extern0 -> E0}}
  lowerThan: Extern0
}
