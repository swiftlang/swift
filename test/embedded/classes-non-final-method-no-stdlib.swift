// RUN: %target-swift-emit-ir -verify %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo

// REQUIRES: swift_in_compiler

public class MyClass {
  func foo<T>(t: T) { } // expected-error {{classes cannot have non-final generic fuctions in embedded Swift}}
  func bar() { }
}

final class C2<Element> {
  // TODO: this shouldn't be a problem because the class is final
  init<T>(x: T) { } // expected-error {{classes cannot have non-final generic fuctions in embedded Swift}}
}

struct S {}

func testit2() -> C2<S> {
  return C2(x: S())
}

open class C3<X> {
  public func foo<T>(t: T) {} // expected-error {{classes cannot have non-final generic fuctions in embedded Swift}}
}

func testit3() -> C3<S> {
  return C3<S>()
}

