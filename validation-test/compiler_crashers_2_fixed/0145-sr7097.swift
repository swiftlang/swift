// RUN: %target-swift-frontend %s -emit-ir -o - | %FileCheck %s

protocol P1 { }

protocol P2 {
  associatedtype Assoc
}

protocol P3 : P2 { }

struct S0<M: P3> where M.Assoc: P1 { }

struct ConformsToP1: P1 { }

extension P3 {
  typealias Assoc = ConformsToP1
}

protocol P5 {
}

extension P5 {
  func testSR7097<M>(_: S0<M>.Type) {}
}


