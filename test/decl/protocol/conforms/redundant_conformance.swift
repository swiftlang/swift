// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/redundant_conformance_A.swift
// RUN: %target-swift-frontend -emit-module -o %t -I %t %S/Inputs/redundant_conformance_B.swift
// RUN: %target-typecheck-verify-swift -I %t %s

import redundant_conformance_A
import redundant_conformance_B

extension ConformsToP
  : P1 { // expected-warning{{conformance of 'ConformsToP' to protocol 'P1' was already stated in the type's module 'redundant_conformance_A'}}
  typealias A = Double // expected-note{{type alias 'A' will not be used to satisfy the conformance to 'P1'}}

  func f() -> Double { return 0.0 } // expected-note{{instance method 'f()' will not be used to satisfy the conformance to 'P1'}}
       // expected-note@-1{{found this candidate}}
}

extension ConformsToP
  : P2 { // expected-warning{{conformance of 'ConformsToP' to protocol 'P2' was already stated in the protocol's module 'redundant_conformance_B'}}
}

extension OtherConformsToP : P1 { // expected-error{{redundant conformance of 'OtherConformsToP' to protocol 'P1'}}
  func f() -> Int { return 0 }
}

func testConformsToP(cp1: ConformsToP, ocp1: OtherConformsToP) {
  // Note:
  let _ = cp1.f()  // expected-error{{ambiguous use of 'f()'}}

  let _ = ocp1.f() // okay: picks "our" OtherConformsToP.f()
}
