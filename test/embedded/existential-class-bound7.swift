// RUN: %target-swift-emit-ir -parse-as-library -module-name main -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public protocol P: AnyObject {
  func foo<T>(t: T)
  func bar()
}

final public class Class: P {
  public func foo<T>(t: T) {}
  public func bar() {}
}

public func testClass() -> P {
  return Class()
}

final public class GenClass<X>: P {
  public func foo<T>(t: T) {}
  public func bar() {}
}

public func testGenClass() -> P {
  return GenClass<Int>()
}

// A non-generic requirement dispatches through the witness table as usual.
public func callBar(p: P) {
  p.bar()
}

// Calling the generic requirement is still an error.
public func callFoo(p: P) {
  p.foo(t: 0)
  // expected-warning@-1 {{cannot use generic instance method 'foo(t:)' on a value of type 'any P' in Embedded Swift}}
  // expected-error@-2 {{a protocol type cannot contain a generic method 'foo(t:)' in embedded Swift}}
}
