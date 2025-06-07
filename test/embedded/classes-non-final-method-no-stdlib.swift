// RUN: %target-swift-emit-ir -verify %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

public class MyClass {
  public func foo<T>(t: T) { } // expected-error {{classes cannot have a non-final, generic method 'foo(t:)' in embedded Swift}}
  public func bar() { }
}

final class C2<Element> {
  init<T>(x: T) { }
}

struct S {}

public func testMyClass(_ c: MyClass) {
  c.foo(t: S())
  c.bar()
}

func testit2() -> C2<S> {
  return C2(x: S())
}

open class C3<X> {
  public func foo<T>(t: T) {} // expected-error {{classes cannot have a non-final, generic method 'foo(t:)' in embedded Swift}}
}

func testit3() -> C3<S> {
  let c = C3<S>()
  c.foo(t: S())
  return c
}

