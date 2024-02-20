// RUN: %target-swift-emit-ir -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu

public func sink<T>(t: T) {}

public func test() -> Int {
  let metatype = Int.self
  sink(t: metatype)
  return 42
}
