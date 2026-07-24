// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name UsePrimitives -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/uses.h
// RUN: %FileCheck %s < %t/uses.h

// RUN: %check-interop-cxx-header-in-clang(%t/uses.h -Wno-reserved-identifier -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: PTRSIZE=64

public struct UsesUnicodeScalar {
    public var scalars: [Unicode.Scalar]
    public var maybeScalar: Unicode.Scalar?
}

public struct UsesFloat16 {
    public var values: [Float16]
    public var maybeValue: Float16?
}

// The trait specializations must be emitted so that the swift::Array /
// swift::Optional instantiations below satisfy their internal static_assert.

// CHECK: inline const constexpr bool isUsableInGenericContext<char32_t> = true;
// CHECK: inline const constexpr bool isUsableInGenericContext<_Float16> = true;

// CHECK: swift::Array<_Float16> getValues() const
// CHECK: swift::Optional<_Float16> getMaybeValue() const
// CHECK: swift::Array<char32_t> getScalars() const
// CHECK: swift::Optional<char32_t> getMaybeScalar() const
