// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/Test.swiftinterface) -module-name Test %s -experimental-lazy-typecheck -verify

extension DoesNotExist { // expected-error {{cannot find type 'DoesNotExist' in scope}}
  public func method() {}
}
