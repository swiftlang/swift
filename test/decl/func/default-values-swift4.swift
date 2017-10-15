// RUN: %target-typecheck-verify-swift -swift-version 4
// RUN: %target-typecheck-verify-swift -swift-version 4 -enable-testing

private func privateFunction() {}
// expected-note@-1 4{{global function 'privateFunction()' is not public}}
fileprivate func fileprivateFunction() {}
// expected-note@-1 4{{global function 'fileprivateFunction()' is not public}}
func internalFunction() {}
// expected-note@-1 4{{global function 'internalFunction()' is not public}}
@_versioned func versionedFunction() {}
public func publicFunction() {}

func internalIntFunction() -> Int {}
// expected-note@-1 2{{global function 'internalIntFunction()' is not public}}

func internalFunctionWithDefaultValue(
    x: Int = {
      struct Nested {}
      // OK

      publicFunction()
      // OK
      versionedFunction()
      // OK
      internalFunction()
      // OK
      fileprivateFunction()
      // OK
      privateFunction()
      // OK

      return 0
    }(),
    y: Int = internalIntFunction()) {}

@_versioned func versionedFunctionWithDefaultValue(
    x: Int = {
      struct Nested {}
      // expected-error@-1 {{type 'Nested' cannot be nested inside a default argument value}}

      // FIXME: Some errors below are diagnosed twice

      publicFunction()
      // OK
      versionedFunction()
      // OK
      internalFunction()
      // expected-error@-1 2{{global function 'internalFunction()' is internal and cannot be referenced from a default argument value}}
      fileprivateFunction()
      // expected-error@-1 2{{global function 'fileprivateFunction()' is fileprivate and cannot be referenced from a default argument value}}
      privateFunction()
      // expected-error@-1 2{{global function 'privateFunction()' is private and cannot be referenced from a default argument value}}

      return 0
    }(),
    y: Int = internalIntFunction()) {}
    // expected-error@-1 {{global function 'internalIntFunction()' is internal and cannot be referenced from a default argument value}}

public func publicFunctionWithDefaultValue(
    x: Int = {
      struct Nested {}
      // expected-error@-1 {{type 'Nested' cannot be nested inside a default argument value}}

      // FIXME: Some errors below are diagnosed twice

      publicFunction()
      // OK
      versionedFunction()
      // OK
      internalFunction()
      // expected-error@-1 2{{global function 'internalFunction()' is internal and cannot be referenced from a default argument value}}
      fileprivateFunction()
      // expected-error@-1 2{{global function 'fileprivateFunction()' is fileprivate and cannot be referenced from a default argument value}}
      privateFunction()
      // expected-error@-1 2{{global function 'privateFunction()' is private and cannot be referenced from a default argument value}}

      return 0
    }(),
    y: Int = internalIntFunction()) {}
    // expected-error@-1 {{global function 'internalIntFunction()' is internal and cannot be referenced from a default argument value}}

// https://bugs.swift.org/browse/SR-5559
public class MyClass {
  public func method<T>(_: T.Type = T.self) -> T { }
}
