// RUN: %target-typecheck-verify-swift -swift-version 4
// RUN: %target-typecheck-verify-swift -swift-version 4 -enable-testing

private func privateFunction() {}
// expected-note@-1 2{{global function 'privateFunction()' is not public}}
fileprivate func fileprivateFunction() {}
// expected-note@-1 2{{global function 'fileprivateFunction()' is not public}}
func internalFunction() {}
// expected-note@-1 2{{global function 'internalFunction()' is not public}}
@usableFromInline func versionedFunction() {}
// expected-note@-1 5{{global function 'versionedFunction()' is not public}}
public func publicFunction() {}

func internalIntFunction() -> Int {}
// expected-note@-1 {{global function 'internalIntFunction()' is not public}}

private func privateFunction2() {}
// expected-note@-1 2{{global function 'privateFunction2()' is not '@usableFromInline' or public}}
fileprivate func fileprivateFunction2() {}
// expected-note@-1 2{{global function 'fileprivateFunction2()' is not '@usableFromInline' or public}}
func internalFunction2() {}
// expected-note@-1 2{{global function 'internalFunction2()' is not '@usableFromInline' or public}}

func internalIntFunction2() -> Int {}
// expected-note@-1 {{global function 'internalIntFunction2()' is not '@usableFromInline' or public}}

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

@usableFromInline func versionedFunctionWithDefaultValue(
    x: Int = {
      struct Nested {}
      // expected-error@-1 {{type 'Nested' cannot be nested inside a default argument value}}

      // FIXME: Some errors below are diagnosed twice

      publicFunction()
      // OK
      versionedFunction()
      // OK
      internalFunction2()
      // expected-error@-1 2{{global function 'internalFunction2()' is internal and cannot be referenced from a default argument value}}
      fileprivateFunction2()
      // expected-error@-1 2{{global function 'fileprivateFunction2()' is fileprivate and cannot be referenced from a default argument value}}
      privateFunction2()
      // expected-error@-1 2{{global function 'privateFunction2()' is private and cannot be referenced from a default argument value}}

      return 0
    }(),
    y: Int = internalIntFunction2()) {}
    // expected-error@-1 {{global function 'internalIntFunction2()' is internal and cannot be referenced from a default argument value}}

public func publicFunctionWithDefaultValue(
    x: Int = {
      struct Nested {}
      // expected-error@-1 {{type 'Nested' cannot be nested inside a default argument value}}

      // FIXME: Some errors below are diagnosed twice

      publicFunction()

      versionedFunction()
      // expected-error@-1 2{{global function 'versionedFunction()' is internal and cannot be referenced from a default argument value}}

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

public func evilCode(
  x: Int = {
    let _ = publicFunction()
    let _ = versionedFunction()
    // expected-error@-1 2{{global function 'versionedFunction()' is internal and cannot be referenced from a default argument value}}

    func localFunction() {
      publicFunction()
      versionedFunction()
      // expected-error@-1 {{global function 'versionedFunction()' is internal and cannot be referenced from a default argument value}}
    }
  }()) {}
