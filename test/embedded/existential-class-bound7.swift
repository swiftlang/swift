// RUN: %target-swift-emit-ir -parse-as-library -module-name main -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu

public protocol P: AnyObject {
  func foo<T>(t: T)
}

final public class Class: P {
  public func foo<T>(t: T) {}
}


public func testClass() -> P {
  return Class() // expected-error {{an existential type cannot contain a generic method 'foo(t:)' in embedded Swift}}
}

final public class GenClass<X>: P {
  public func foo<T>(t: T) {}
}


public func testGenClass() -> P {
  return GenClass<Int>() // expected-error {{an existential type cannot contain a generic method 'foo(t:)' in embedded Swift}}
}

