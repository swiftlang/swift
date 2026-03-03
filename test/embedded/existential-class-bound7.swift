// RUN: %target-swift-emit-ir -parse-as-library -module-name main -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public protocol P: AnyObject {
  func foo<T>(t: T)
}

final public class Class: P {
  public func foo<T>(t: T) {} // expected-error {{a protocol type cannot contain a generic method 'foo(t:)' in embedded Swift}}
}


public func testClass() -> P {
  return Class() // expected-note {{protocol type value created here}}
}

final public class GenClass<X>: P {
  public func foo<T>(t: T) {} // expected-error {{a protocol type cannot contain a generic method 'foo(t:)' in embedded Swift}}
}


public func testGenClass() -> P {
  return GenClass<Int>() // expected-note {{protocol type value created here}}
}

