// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded -o /dev/null

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=linux-gnu || OS=windows-msvc || OS=windows-msvc

// Check that this doesn't crash the compiler in embedded mode.

public func foo<T>(_ t: T) -> T {
  t
}

public func testit(_ x: Int = foo(27)) -> Int {
  x
}

public func callit() -> Int {
  testit()
}
