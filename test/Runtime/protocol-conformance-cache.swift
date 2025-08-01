// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: env SWIFT_DEBUG_ENABLE_PROTOCOL_CONFORMANCES_LOOKUP_LOG=1 %target-run %t/a.out 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_stdlib_asserts
// UNSUPPORTED: DARWIN_SIMULATOR=ios
// UNSUPPORTED: DARWIN_SIMULATOR=tvos
// UNSUPPORTED: DARWIN_SIMULATOR=watchos
// UNSUPPORTED: DARWIN_SIMULATOR=xros
// UNSUPPORTED: use_os_stdlib

// The optimizer will remove many of these conformance checks due to statically
// knowing the result.
// UNSUPPORTED: swift_test_mode_optimize
// UNSUPPORTED: swift_test_mode_optimize_size

protocol Proto {}
extension Proto {
  static var selfType: Any.Type { Self.self }
}

func conformsToProto<T>(_ type: T.Type) -> Bool {
  (type as? Proto.Type)?.selfType == type
}

func doesNotConformToProto<T>(_ type: T) -> Bool {
  (type as? Proto.Type) == nil
}

extension Array: Proto {}
extension Dictionary: Proto where Key: Proto {}
extension Int: Proto {}

// CHECK: Check confomance Swift.Int to Proto: found, source: section scan
assert(conformsToProto(Int.self))
// CHECK: Check confomance Swift.Int to Proto: found, source: cache by type metadata
assert(conformsToProto(Int.self))

// CHECK: Check confomance Swift.String to Proto: not found, source: section scan
assert(doesNotConformToProto(String.self))
// CHECK: Check confomance Swift.String to Proto: not found, source: cache by type metadata
assert(doesNotConformToProto(String.self))

// CHECK: Check confomance Swift.Array<Swift.Int> to Proto: found, source: section scan
assert(conformsToProto([Int].self))
// CHECK: Check confomance Swift.Array<Swift.Int> to Proto: found, source: cache by type metadata
assert(conformsToProto([Int].self))

// CHECK: Check confomance Swift.Array<Swift.String> to Proto: found, source: cache by type descriptor
assert(conformsToProto([String].self))
// CHECK: Check confomance Swift.Array<Swift.String> to Proto: found, source: cache by type metadata
assert(conformsToProto([String].self))

// CHECK: Check confomance Swift.Dictionary<Swift.Int, Swift.Int> to Proto: found, source: section scan
assert(conformsToProto([Int: Int].self))

// CHECK: Check confomance Swift.Dictionary<Swift.String, Swift.Int> to Proto: not found, source: cache by type descriptor
assert(doesNotConformToProto([String: Int].self))
// CHECK: Check confomance Swift.Dictionary<Swift.String, Swift.Int> to Proto: not found, source: cache by type metadata
assert(doesNotConformToProto([String: Int].self))


class BaseClass: Proto {}
class DerivedClass: BaseClass {}
class GenericClass<T>: Proto {}
class GenericClassConditional<T> {}
extension GenericClassConditional: Proto where T: Proto {}

// CHECK: Check confomance a.BaseClass to Proto: found, source: section scan
assert(conformsToProto(BaseClass.self))
// CHECK: Check confomance a.BaseClass to Proto: found, source: cache by type metadata
assert(conformsToProto(BaseClass.self))

// CHECK: Check confomance a.DerivedClass to Proto: found, source: section scan
assert(conformsToProto(DerivedClass.self))
// CHECK: Check confomance a.DerivedClass to Proto: found, source: cache by type metadata
assert(conformsToProto(DerivedClass.self))

// CHECK: Check confomance a.GenericClass<Swift.Int> to Proto: found, source: section scan
assert(conformsToProto(GenericClass<Int>.self))
// CHECK: Check confomance a.GenericClass<Swift.Int> to Proto: found, source: cache by type metadata
assert(conformsToProto(GenericClass<Int>.self))

// CHECK: Check confomance a.GenericClass<Swift.String> to Proto: found, source: cache by type descriptor
assert(conformsToProto(GenericClass<String>.self))
// CHECK: Check confomance a.GenericClass<Swift.String> to Proto: found, source: cache by type metadata
assert(conformsToProto(GenericClass<String>.self))

// CHECK: Check confomance a.GenericClassConditional<Swift.Int> to Proto: found, source: section scan
assert(conformsToProto(GenericClassConditional<Int>.self))
// CHECK: Check confomance a.GenericClassConditional<Swift.Int> to Proto: found, source: cache by type metadata
assert(conformsToProto(GenericClassConditional<Int>.self))

// CHECK: Check confomance a.GenericClassConditional<Swift.String> to Proto: not found, source: cache by type descriptor
assert(doesNotConformToProto(GenericClassConditional<String>.self))
// CHECK: Check confomance a.GenericClassConditional<Swift.String> to Proto: not found, source: cache by type metadata
assert(doesNotConformToProto(GenericClassConditional<String>.self))

enum Enum: Proto {}
extension Optional: Proto where Wrapped: Proto {}

// CHECK: Check confomance a.Enum to Proto: found, source: section scan
assert(conformsToProto(Enum.self))
// CHECK: Check confomance a.Enum to Proto: found, source: cache by type metadata
assert(conformsToProto(Enum.self))

// CHECK: Check confomance Swift.Optional<a.Enum> to Proto: found, source: section scan
assert(conformsToProto(Enum?.self))
// CHECK: Check confomance Swift.Optional<a.Enum> to Proto: found, source: cache by type metadata
assert(conformsToProto(Enum?.self))

// CHECK: Check confomance Swift.Optional<Swift.Int> to Proto: found, source: cache by type descriptor
assert(conformsToProto(Int?.self))
// CHECK: Check confomance Swift.Optional<Swift.Int> to Proto: found, source: cache by type metadata
assert(conformsToProto(Int?.self))

// CHECK: Check confomance Swift.Optional<Swift.String> to Proto: not found, source: cache by type descriptor
assert(doesNotConformToProto(String?.self))
// CHECK: Check confomance Swift.Optional<Swift.String> to Proto: not found, source: cache by type metadata
assert(doesNotConformToProto(String?.self))
