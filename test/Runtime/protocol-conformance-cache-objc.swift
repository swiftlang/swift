// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: env %env-SWIFT_DEBUG_ENABLE_PROTOCOL_CONFORMANCES_LOOKUP_LOG=1 %target-run %t/a.out 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_stdlib_asserts
// UNSUPPORTED: DARWIN_SIMULATOR=ios
// UNSUPPORTED: DARWIN_SIMULATOR=tvos
// UNSUPPORTED: DARWIN_SIMULATOR=watchos
// UNSUPPORTED: DARWIN_SIMULATOR=xros
// UNSUPPORTED: use_os_stdlib

import Foundation

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

@objc class BaseClass: NSObject, Proto {}
@objc class DerivedClass: BaseClass {}
@objc class NonConformingClass: NSObject {}

// CHECK: Check confomance a.BaseClass to Proto: found, source: section scan
assert(conformsToProto(BaseClass.self))
// CHECK: Check confomance a.BaseClass to Proto: found, source: cache by type metadata
assert(conformsToProto(BaseClass.self))

// CHECK: Check confomance a.DerivedClass to Proto: found, source: section scan
assert(conformsToProto(DerivedClass.self))
// CHECK: Check confomance a.DerivedClass to Proto: found, source: cache by type metadata
assert(conformsToProto(DerivedClass.self))

// CHECK: Check confomance a.NonConformingClass to Proto: not found, source: section scan
assert(doesNotConformToProto(NonConformingClass.self))
// CHECK: Check confomance a.NonConformingClass to Proto: not found, source: cache by type metadata
assert(doesNotConformToProto(NonConformingClass.self))
