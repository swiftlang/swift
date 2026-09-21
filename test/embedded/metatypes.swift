// RUN: %target-swift-emit-ir -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public func sink<T>(t: T) {}

public func test() -> Int {
  let metatype = Int.self
  sink(t: metatype)
  return 42
}

func castToExistential<T>(x: T) {
  // A cast to a type involving a protocol is rejected during type checking, so
  // compilation stops before the SIL-level check would run.
  if x is any FixedWidthInteger {    // expected-error {{cannot perform a dynamic cast to a type involving protocol 'FixedWidthInteger' in Embedded Swift}}
  }
}

public func callCastToExistential() {
  castToExistential(x: 42)
}

