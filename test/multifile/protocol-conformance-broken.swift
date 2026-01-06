// RUN: %target-swift-frontend -typecheck -whole-module-optimization -verify %s %S/Inputs/protocol-conformance/broken.swift

// rdar://problem/29689007
protocol P {
  associatedtype AT // expected-note{{protocol requires nested type 'AT'}}
  func f() -> AT
}

struct X { }

func g() {
  let _: X.AT? = nil // expected-error{{reference to invalid associated type 'AT' of type 'X'}}
}

