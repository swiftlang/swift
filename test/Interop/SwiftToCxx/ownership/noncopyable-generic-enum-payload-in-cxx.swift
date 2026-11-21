// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Payload -typecheck -verify -cxx-interoperability-mode=default -I %S/Inputs -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX -clang-header-expose-decls=all-public -emit-clang-header-path %t/payload.h
// RUN: %FileCheck %s < %t/payload.h
// RUN: echo '#include "cxx-move-only.h"' > %t/combined.h
// RUN: cat %t/payload.h >> %t/combined.h
// RUN: %check-interop-cxx-header-in-clang(-I %S/Inputs %t/combined.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: swift_feature_GenerateBindingsForNoncopyableTypesInCXX

import CxxMoveOnly

public struct Conditional<A: ~Copyable, B>: ~Copyable {
  public var value: Int
  public init(_ value: Int) { self.value = value }
}
extension Conditional: Copyable where A: Copyable {}

// The declaration can be Copyable, but this payload specialization cannot.
// Its case constructor must not copy it.
public enum HoldsConditional<T>: ~Copyable {
  case payload(Conditional<CxxMoveOnly, T>)
}

// A copyable specialization is still a supported payload.
public enum HoldsCopyableConditional<T>: ~Copyable {
  case payload(Conditional<Int, T>)
}

// Keep conditionally copyable enums available for copyable specializations,
// even though their payload type is not Copyable in the generic context.
public enum OptionalLike<T: ~Copyable>: ~Copyable {
  case none
  case some(T)
}
extension OptionalLike: Copyable where T: Copyable {}

public func makeOptional() -> Int? { nil }

// CHECK-DAG: class HoldsConditional { } SWIFT_UNAVAILABLE_MSG("Swift generic enum 'HoldsConditional' cannot be represented in C++");
// CHECK-DAG: class SWIFT_SYMBOL({{.*}}) HoldsCopyableConditional final {
// CHECK-DAG: SWIFT_INLINE_THUNK HoldsCopyableConditional<T_0_0> operator()(const Conditional<swift::Int, T_0_0>& val) const;
// CHECK-DAG: class SWIFT_SYMBOL({{.*}}) OptionalLike final {
// CHECK-DAG: SWIFT_INLINE_THUNK OptionalLike<T_0_0> operator()(const T_0_0& val) const;
// CHECK-DAG: class SWIFT_SYMBOL("s:Sq") Optional final {
// CHECK-DAG: SWIFT_INLINE_THUNK swift::Optional<swift::Int> makeOptional() noexcept
