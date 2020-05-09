// RUN: %target-swift-frontend -O %s -emit-ir -o /dev/null

// Check that we don't crash here.

enum E<T> {
  case A
  case B(T)
}

public protocol P {
  associatedtype A
}

internal class Base<T: P> {
  func foo() -> E<T.A> {
    return .A
  }
}

struct Outer<T: P> where T.A == Int {
  private class Inner : Base<T> {

    // This overridden function has a different ABI than the base implementation.
    // The BasicCalleeAnalysis put both methods in the callee list, which let
    // some optimizations crash.
    override func foo() -> E<T.A> {
      return .A
    }
  }
}

@inline(never)
func getit<T: P>(_ t: T) -> Base<T> {
  return Base<T>()
}

public func testit<T: P>(_ t: T) {
  let b = getit(t)
  b.foo()
}

