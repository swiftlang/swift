// RUN: %target-swift-emit-ir -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public func sink<T>(t: T) {}

public func test() -> Int {
  let metatype = Int.self
  sink(t: metatype)
  return 42
}

func castToExistential<T>(x: T) {
  if x is any FixedWidthInteger {    // expected-error {{cannot do dynamic casting in embedded Swift}}
  }
}

public func callCastToExistential() {
  castToExistential(x: 42)    // expected-note {{generic specialization called here}}
}

