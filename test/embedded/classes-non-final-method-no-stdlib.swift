// A generic method of a class is dispatched statically in Embedded Swift and
// kept out of the vtable, so a non-final one is fine as long as nothing can
// override it. None of these are `open` or overridden, so all of them are
// accepted -- this file used to be entirely diagnostics.

// RUN: %target-swift-emit-ir -verify %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo

// REQUIRES: swift_feature_Embedded

public class MyClass {
  public func foo<T>(t: T) { }
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

// `open` class, but the generic method is only `public`, so it cannot be
// overridden from another module.
open class C3<X> {
  public func foo<T>(t: T) {}
}

func testit3() -> C3<S> {
  let c = C3<S>()
  c.foo(t: S())
  return c
}
