// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Consume -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/consume.h
// RUN: %FileCheck %s < %t/consume.h
// RUN: %FileCheck --check-prefix=NOSELF %s < %t/consume.h
// RUN: %check-interop-cxx-header-in-clang(%t/consume.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: swift_feature_GenerateBindingsForNoncopyableTypesInCXX

// Whether C++ has to move a value into Swift follows the lowered parameter
// convention, not the specifier written in the source.

public struct MO: ~Copyable {
    public let x: Int
    public init(x: Int) { self.x = x }
}

public struct Copyable1 {
    public let x: Int
    public init(x: Int) { self.x = x }
}

// 'consuming' spelled out.
public func consumeMO(_ s: consuming MO) {}

// Consumed by convention, without the 'consuming' keyword.
public func takeOwnedMO(_ s: __owned MO) {}

// A copyable consumed parameter is still copied, so it stays a const reference.
public func consumeCopyable(_ s: consuming Copyable1) {}

// An 'inout' parameter is neither borrowed nor consumed.
public func mutateMO(_ s: inout MO) {}

public struct Holder: ~Copyable {
    // A setter consumes 'newValue' without spelling it 'consuming'.
    public var value: MO

    public init(value: consuming MO) { self.value = value }
}

// Consuming 'self' would need an rvalue-reference-qualified member function to
// move out of the C++ object, so neither spelling is exposed yet.
extension MO {
    public consuming func consumeSelf() -> Int { return x }
    public __consuming func legacyConsumeSelf() -> Int { return x }
}

// CHECK: SWIFT_INLINE_THUNK void consumeCopyable(const Copyable1& s) noexcept

// CHECK: SWIFT_INLINE_THUNK void consumeMO(MO&& s) noexcept
// CHECK-NEXT: alignas(alignof(MO)) char copyBuffer_consumedParamCopy_s[sizeof(MO)];
// CHECK-NEXT: auto &consumedParamCopy_s = *(new(copyBuffer_consumedParamCopy_s) MO(static_cast<MO &&>(s)));

// An 'inout' parameter stays a mutable lvalue reference and is not moved from.
// CHECK: SWIFT_INLINE_THUNK void mutateMO(MO& s) noexcept
// CHECK-NOT: copyBuffer_consumedParamCopy_s

// CHECK: SWIFT_INLINE_THUNK void takeOwnedMO(MO&& s) noexcept

// CHECK: SWIFT_INLINE_THUNK void Holder::setValue(MO&& value)

// CHECK: SWIFT_INLINE_THUNK Holder Holder::init(MO&& value)

// A getter would have to copy the property out of a borrowed 'self'.
// NOSELF-NOT: getValue

// Neither consuming-'self' method is exposed, under either spelling. Checked
// under its own prefix, so that it covers the whole header.
// NOSELF-NOT: consumeSelf
// NOSELF-NOT: legacyConsumeSelf
