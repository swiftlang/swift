// RUN: %target-swift-emit-ir -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

public func sink<T>(t: T) {}

public func test() -> Int {
  let metatype = Int.self
  sink(t: metatype) // expected-error {{cannot use metatype of type 'Int' in embedded Swift}}
  // expected-note@-1 {{called from here}}
  return 42
}
