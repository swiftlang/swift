// RUN: %target-swift-emit-ir -parse-as-library -module-name main -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public func formExistential(_ x: [Int]) -> any Sequence<Int> {
  return x
}

public func callGenericRequirement(_ s: any Sequence<Int>) -> Int? {
  return s.withContiguousStorageIfAvailable { $0.count }
  // expected-warning@-1 {{cannot use generic instance method 'withContiguousStorageIfAvailable' on a value of type 'any Sequence<Int>' in Embedded Swift}}
  // expected-error@-2 {{a protocol type cannot contain a generic method 'withContiguousStorageIfAvailable' in embedded Swift}}
}
