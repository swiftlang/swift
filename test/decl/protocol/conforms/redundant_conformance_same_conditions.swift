// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/redundant_conformance_A.swift
// RUN: %target-swift-frontend -emit-module -o %t -I %t %S/Inputs/redundant_conformance_B.swift
// RUN: %target-typecheck-verify-swift -I %t

import redundant_conformance_A
import redundant_conformance_B

// These have identical requirements to the original conformances

extension GenericConditionalConformsToP: P1 where T == Int {
    // expected-warning@-1{{conformance of 'GenericConditionalConformsToP<T>' to protocol 'P1' was already stated in the type's module 'redundant_conformance_A'}}
    func f() -> Int { return 0 }
    // expected-note@-1{{instance method 'f()' will not be used to satisfy the conformance to 'P1'}}
}
extension GenericConditionalConformsToP: P2 where T: P1 {
    // expected-warning@-1{{conformance of 'GenericConditionalConformsToP<T>' to protocol 'P2' was already stated in the protocol's module 'redundant_conformance_B'}}
    func f() -> Int { return 0 }
    // expected-note@-1{{instance method 'f()' will not be used to satisfy the conformance to 'P2'}}
}
extension OtherGenericConditionalConformsToP: P1 where T: P2 {
    // expected-error@-1{{redundant conformance of 'OtherGenericConditionalConformsToP<T>' to protocol 'P1'}}
    func f() -> Int { return 0 }
}
