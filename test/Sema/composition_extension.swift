// RUN: %target-typecheck-verify-swift
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -primary-file %s %S/Inputs/composition_extension_usage.swift -emit-module-path %t/P-partial.swiftmodule -module-name SR11227 -enable-testing
// RUN: %target-swift-frontend -primary-file %S/Inputs/composition_extension_usage.swift %s -emit-module-path %t/S-partial.swiftmodule -module-name SR11227 -enable-testing
// RUN: %target-swift-frontend -sil-merge-partial-modules %t/P-partial.swiftmodule %t/S-partial.swiftmodule -emit-module -o %t/SR11227.swiftmodule

protocol P1 {}

protocol P1_1 : P1 {
  func p1_1()
}

protocol P2 {}

extension P1 & P1_1 where Self : P2 {
  // expected-warning@-1 {{extending a protocol composition is not supported; extending 'P1' instead}}
  // expected-note@-2 {{did you mean to extend the most specific type 'P1_1' instead?}} {{11-20=P1_1}}
  func p1_1() {}
}

extension P1_1 & P1 where Self : P2 {
  // expected-warning@-1 {{extending a protocol composition is not supported; extending 'P1_1' instead}} {{11-20=P1_1}}
  func p1_1_alt() {}
}

typealias T1 = P1 & P1_1

extension T1 {
  // expected-warning@-1 {{extending a protocol composition is not supported; extending 'P1' instead}}
  // expected-note@-2 {{did you mean to extend the most specific type 'P1_1' instead?}} {{11-13=P1_1}}
  func t1() {}
}

typealias T2 = T1

extension T2 {
  // expected-warning@-1 {{extending a protocol composition is not supported; extending 'P1' instead}}
  // expected-note@-2 {{did you mean to extend the most specific type 'P1_1' instead?}} {{11-13=P1_1}}
  func t2() {}
}

typealias T3 = P1_1 & P1

extension T3 { // Ideally, we should emit a warning here but the current implementation doesn't do that.
    func t3() {}
}
