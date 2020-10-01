// RUN: %target-swift-frontend -emit-ir -O %s | %FileCheck %s

// This is a compile-only test. It checks that the compiler does not crash for
// a partial_apply with "unusual" conformances: rdar://problem/59456064

public struct Inner<Element> { }

extension Inner: Equatable where Element: Equatable {
  public static func == (lhs: Inner<Element>, rhs: Inner<Element>) -> Bool {
    return false
  }
}

public struct Outer<Value> { }

extension Outer: Equatable where Value: Equatable {
  public static func == (lhs: Outer<Value>, rhs: Outer<Value>) -> Bool {
    return false
  }
}

@inline(never)
func foo<Element>(_ x: Element, _ equal: (Element, Element) -> Bool) {
  _ = equal(x, x)
}

// CHECK: define {{.*}}testit
public func testit<Element: Equatable>(_ x: Outer<Inner<Element>>) {
  foo(x, ==)
}

