// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -primary-file %s %S/Inputs/composition_extensions_usage.swift -emit-module-path %t/P-partial.swiftmodule -module-name SR11227 -enable-testing
// RUN: %target-swift-frontend -primary-file %S/Inputs/composition_extensions_usage.swift %s -emit-module-path %t/S-partial.swiftmodule -module-name SR11227 -enable-testing
// RUN: %target-swift-frontend -sil-merge-partial-modules %t/P-partial.swiftmodule %t/S-partial.swiftmodule -emit-module -o %t/SR11227.swiftmodule

// SR11227 test case

protocol SR11227_P0 {}

protocol SR11227_P1 {}

protocol SR11227_P1_1 : SR11227_P1 {
  func sr11227_p1_1()
}

protocol SR11227_P2 {}

extension SR11227_P1 & SR11227_P1_1 where Self : SR11227_P2 {
  func sr11227_p1_1() {}
}

// Common definitions

class C1        { func c1() {} }

class C1_1 : C1 { func c1_1() {} }

class C2        { func c2() {} }

class C3        { func c3() {} }

protocol P1 { func p1() }

protocol P1_1 : P1 { func p1_1() }

extension C1_1 : P1 { func p1() {} }

typealias T1 = P1

protocol P2 { func p2() }

extension C1_1 : P2 { func p2() {} }

typealias T2 = P2

protocol P3 { func p3() }

protocol P4 { func p4() }

// Basics

extension P1 & C1 where Self : P2 {
  func c1_p1_p2() { self.c1(); self.p1(); self.p2() }
}

func works() {
  C1_1.init().c1_p1_p2()
}

// Most specific

extension P1_1 & P1 {
  func p1_p1_1() { self.p1(); self.p1_1() }
}

// Typealiases

extension P1 & T1 & T2 {
  func p1_p3_p2() {
    self.p1()
    self.p2()
  }
}

// Nested + typealiases

extension P1 & ((T2 & C1_1) & T1) {
  func c1_1_p1_p4() { self.c1_1(); self.p1(); self.p2() }
}

// Nested + Most specific

extension P1_1 & C1 & (P4 & P1 & C1) {
  func c1_p1_p1_1_p4() { self.c1(); self.p1(); self.p1_1(); self.p4() }
}

// Composition typealiases

typealias T_P1_C2 = P1 & C2
extension T_P1_C2 & P2 {
  func c2_p1_p2() { self.c2(); self.p1(); self.p2() }
}

// Medley

typealias T_C2_P3 = C2 & P3
typealias T_C2_P3_P4 = T_C2_P3 & P4
typealias T_P1_P4 = T1 & P4

extension (T_C2_P3 & T_P1_P4) & T_C2_P3_P4 {
  func c2_p1_p3_p4() { self.c2(); self.p1(); self.p3(); self.p4() }
}
