// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules -I %t %s -verify

import cfuncs

func testNonexistentPowerOperatorWithPowFunctionInScope() {
  func a(_ value: Double) { }
  let x: Double = 3.0
  let y: Double = 3.0
  let z: Double = x**y // expected-error {{no operator '**' is defined; did you mean 'pow(_:_:)'?}}
  let w: Double = a(x**2.0) // expected-error {{no operator '**' is defined; did you mean 'pow(_:_:)'?}}
}
