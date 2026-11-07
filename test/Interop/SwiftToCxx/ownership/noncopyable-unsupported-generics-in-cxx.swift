// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -target %target-swift-6.2-abi-triple -module-name UnsupportedGenerics -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/unsupported-generics.h
// RUN: %FileCheck %s < %t/unsupported-generics.h
// RUN: %check-interop-cxx-header-in-clang(%t/unsupported-generics.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// RUN: %target-swift-frontend %s -target %target-swift-6.2-abi-triple -module-name UnsupportedGenerics -enable-library-evolution -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/unsupported-generics-evo.h
// RUN: %FileCheck %s < %t/unsupported-generics-evo.h
// RUN: %check-interop-cxx-header-in-clang(%t/unsupported-generics-evo.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: swift_feature_GenerateBindingsForNoncopyableTypesInCXX

// The C++ header printer cannot emit parameter packs or value parameters yet,
// even when they do not affect the layout.
public struct PackMO<each T>: ~Copyable {
  public var n: Int
  public init(n: Int) { self.n = n }
}

public struct ValueMO<let N: Int>: ~Copyable {
  public var n: Int
  public init(n: Int) { self.n = n }
}

public enum ValueEnumMO<let N: Int>: ~Copyable {
  case value(Int)
}

// An ordinary type parameter remains supported.
public struct ScalarMO<T>: ~Copyable {
  public var value: T
  public init(value: T) { self.value = value }
}

// CHECK-DAG: class PackMO { } SWIFT_UNAVAILABLE_MSG("non-'Copyable' generic struct 'PackMO' can not yet be represented in C++");
// CHECK-DAG: class ValueMO { } SWIFT_UNAVAILABLE_MSG("non-'Copyable' generic struct 'ValueMO' can not yet be represented in C++");
// CHECK-DAG: class ValueEnumMO { } SWIFT_UNAVAILABLE_MSG("non-'Copyable' generic enum 'ValueEnumMO' can not yet be represented in C++");
// CHECK-DAG: class SWIFT_SYMBOL({{.*}}) ScalarMO final {
