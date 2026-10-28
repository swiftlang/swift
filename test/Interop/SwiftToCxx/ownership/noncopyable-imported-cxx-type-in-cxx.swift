// A noncopyable type imported from C++ keeps its own C++ identity: it is not
// re-synthesized as a Swift move-only class, so it gains no moved-from flag and
// its parameters are not rewritten to rvalue references.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend \
// RUN:   -typecheck -verify %s \
// RUN:   -module-name Imported \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX \
// RUN:   -I %S/Inputs \
// RUN:   -clang-header-expose-decls=all-public \
// RUN:   -emit-clang-header-path %t/imported.h
// RUN: %FileCheck --implicit-check-not=_isMovedFrom %s < %t/imported.h
// RUN: %FileCheck --check-prefix=PAYLOAD %s < %t/imported.h

// RUN: echo '#include "cxx-move-only.h"' > %t/combined.h
// RUN: cat %t/imported.h >> %t/combined.h
// RUN: %check-interop-cxx-header-in-clang(-I %S/Inputs %t/combined.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: swift_feature_GenerateBindingsForNoncopyableTypesInCXX

import CxxMoveOnly

public func borrowCxx(_ x: borrowing CxxMoveOnly) -> Int32 { return x.get() }

public func consumeCxx(_ x: consuming CxxMoveOnly) {}

public func makeCxx() -> CxxMoveOnly { return CxxMoveOnly(5) }

// A noncopyable payload would have to be moved into the case constructor,
// whether it is the imported type itself or an 'Optional' of it.
public enum EnumWithCxxPayload: ~Copyable {
    case a(CxxMoveOnly)
    case b
}

public enum EnumWithOptionalCxxPayload: ~Copyable {
    case a(CxxMoveOnly?)
    case b
}

// PAYLOAD-DAG: class EnumWithCxxPayload { } SWIFT_UNAVAILABLE_MSG("Swift enum 'EnumWithCxxPayload' cannot be represented in C++");
// PAYLOAD-DAG: class EnumWithOptionalCxxPayload { } SWIFT_UNAVAILABLE_MSG("Swift enum 'EnumWithOptionalCxxPayload' cannot be represented in C++");

// CHECK: inline const constexpr bool isUsableInGenericContext<CxxMoveOnly> = true;
// CHECK: inline const constexpr bool isSwiftBridgedCxxRecord<CxxMoveOnly> = true;

// CHECK: SWIFT_INLINE_THUNK int32_t borrowCxx(const CxxMoveOnly& x) noexcept

// The value is moved out of the returned storage by C++'s own move
// constructor, not by a Swift value witness.
// CHECK: SWIFT_INLINE_THUNK CxxMoveOnly makeCxx() noexcept
// CHECK: CxxMoveOnly result(static_cast<CxxMoveOnly &&>(*storageObjectPtr));

// Handing ownership of an imported C++ value to Swift is not supported.
// CHECK: // Unavailable in C++: Swift global function 'consumeCxx(_:)'.
