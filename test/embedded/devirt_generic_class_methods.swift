// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature Embedded -Xllvm -sil-disable-pass=mandatory-inlining -emit-ir -o /dev/null

// REQUIRES: swift_feature_Embedded


// Check that the compiler doesn't crash

public class Base<T> {
  func foo(_ t: T) -> T {
    return t
  }
}

@_transparent
func callee(_ i: Int, _ c: Base<Int>) -> Int {
  return c.foo(i)
}

public func testit(_ i : Int) -> Int {
  return callee(i, Base<Int>())
}

