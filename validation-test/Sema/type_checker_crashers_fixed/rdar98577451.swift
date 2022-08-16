// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

protocol P {
  associatedtype T
  func f() -> T
}

struct S: P {
  func f() -> some Any { return 3 }
}

struct G<T> {
  typealias Foo = (T?, S.T)
}

func f<T>(t: T) -> G<T>.Foo {
  return (t, S().f()) // Ok
}
