// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -verify -emit-module -experimental-allow-module-with-compiler-errors %s -o %t/foo.swiftmodule
// RUN: %target-swift-frontend -verify -emit-module -module-name foo %t/foo.swiftmodule

// rdar://97267326 – Make sure we can handle an operator function without its declaration.
struct S {
  static func ^^^ (lhs: S, rhs: S) {} // expected-error {{operator implementation without matching operator declaration}}
}
