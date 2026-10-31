// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-%target-objc-interop < %t/structs.h
// RUN: %FileCheck %s --check-prefix=FIXED < %t/structs.h
// RUN: %FileCheck %s --check-prefix=OFFSET < %t/structs.h
// RUN: %FileCheck %s --check-prefix=STRING-%target-ptrsize < %t/structs.h
// RUN: %target-swift-frontend %s -module-name Structs -enable-library-evolution -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/resilient.h
// RUN: %FileCheck %s --check-prefixes=FIXED,RESILIENT < %t/resilient.h

// RUN: %check-interop-cxx-header-in-clang(%t/structs.h -Wno-unused-function -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %check-interop-cxx-header-in-clang(%t/resilient.h -Wno-unused-function -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

@usableFromInline
class RefCountedClass {
    init() {
        print("create RefCountedClass")
    }
    deinit {
        print("destroy RefCountedClass")
    }
}

public struct StructWithRefcountedMember {
    let x: RefCountedClass
}

public func returnNewStructWithRefcountedMember() -> StructWithRefcountedMember {
    return StructWithRefcountedMember(x: RefCountedClass())
}

public func printBreak(_ x: Int) {
    print("breakpoint \(x)")
}

public func identity<T>(_ value: T) -> T {
    value
}

// CHECK:      class SWIFT_SYMBOL({{.*}}) StructWithRefcountedMember final {
// CHECK-NEXT: public:
// CHECK-NEXT:   SWIFT_INLINE_THUNK ~StructWithRefcountedMember() noexcept {
// CHECK-NEXT:     void *reference;
// CHECK-NEXT:     memcpy(&reference, _getOpaquePointer(), sizeof(reference));
// CHECK-NEXT:     swift::_impl::swift_release(reference);
// CHECK-NEXT:   }
// CHECK-NEXT:   SWIFT_INLINE_THUNK StructWithRefcountedMember(const StructWithRefcountedMember &other) noexcept {
// CHECK-NEXT:     void *reference;
// CHECK-NEXT:     memcpy(&reference, other._getOpaquePointer(), sizeof(reference));
// CHECK-NEXT:     swift::_impl::swift_retain(reference);
// CHECK-NEXT:     memcpy(_getOpaquePointer(), other._getOpaquePointer(), sizeof(_storage));
// CHECK-NEXT:   }
// CHECK-NEXT:   SWIFT_INLINE_THUNK StructWithRefcountedMember &operator =(const StructWithRefcountedMember &other) noexcept {
// CHECK-NEXT:     void *newReference;
// CHECK-NEXT:     memcpy(&newReference, other._getOpaquePointer(), sizeof(newReference));
// CHECK-NEXT:     swift::_impl::swift_retain(newReference);
// CHECK-NEXT:     void *oldReference;
// CHECK-NEXT:     memcpy(&oldReference, _getOpaquePointer(), sizeof(oldReference));
// CHECK-NEXT:     memmove(_getOpaquePointer(), other._getOpaquePointer(), sizeof(_storage));
// CHECK-NEXT:     swift::_impl::swift_release(oldReference);
// CHECK-NEXT:   return *this;
// CHECK-NEXT:   }
// CHECK-NEXT: private:
// CHECK:      class _impl_StructWithRefcountedMember {
// CHECK:        static SWIFT_INLINE_THUNK void initializeWithTake(char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// CHECK-NEXT:     memcpy(destStorage, srcStorage, {{4|8}});
// CHECK-NEXT:   }

@frozen public struct FrozenReference {
    let value: RefCountedClass
}

// FIXED-LABEL: SWIFT_INLINE_THUNK ~FrozenReference() noexcept {
// FIXED-NEXT:    void *reference;
// FIXED-NEXT:    memcpy(&reference, _getOpaquePointer(), sizeof(reference));
// FIXED-NEXT:    swift::_impl::swift_release(reference);
// FIXED-NEXT:  }

@frozen public struct FrozenResilientReference {
    let value: StructWithRefcountedMember
}

// Even the defining module must not bake a resilient field's ownership into
// its public header.
// RESILIENT-LABEL: SWIFT_INLINE_THUNK ~FrozenResilientReference() noexcept {
// RESILIENT:         vwTable->destroy(_getOpaquePointer(), metadata._0);
// RESILIENT-NEXT:  }

public struct GenericReference<T> {
    let value: RefCountedClass
}

// FIXED-LABEL: SWIFT_INLINE_THUNK ~GenericReference() noexcept {
// FIXED:         vwTable->destroy(_getOpaquePointer(), metadata._0);
// FIXED-NEXT:  }

@frozen public enum ReferenceEnum {
    case value(FrozenReference)
}

public func returnNewReferenceEnum() -> ReferenceEnum {
    .value(FrozenReference(value: RefCountedClass()))
}

// A singleton enum has the ownership operations of its payload.
// FIXED-LABEL: SWIFT_INLINE_THUNK ~ReferenceEnum() noexcept {
// FIXED-NEXT:    void *reference;
// FIXED-NEXT:    memcpy(&reference, _getOpaquePointer(), sizeof(reference));
// FIXED-NEXT:    swift::_impl::swift_release(reference);
// FIXED-NEXT:  }

// RESILIENT-LABEL: SWIFT_INLINE_THUNK ~StructWithRefcountedMember() noexcept {
// RESILIENT:         vwTable->destroy(_getOpaquePointer(), metadata._0);
// RESILIENT-NEXT:  }

public struct UnknownReference {
    let value: AnyObject
}

// AnyObject uses native reference counting when Objective-C interop is disabled.
// CHECK-LABEL: SWIFT_INLINE_THUNK ~UnknownReference() noexcept {
// CHECK-objc:    vwTable->destroy(_getOpaquePointer(), metadata._0);
// CHECK-nonobjc-NEXT: void *reference;
// CHECK-nonobjc-NEXT: memcpy(&reference, _getOpaquePointer(), sizeof(reference));
// CHECK-nonobjc-NEXT: swift::_impl::swift_release(reference);
// CHECK-NEXT:  }

public struct UnownedReference {
    unowned let value: RefCountedClass
}

// CHECK-LABEL: SWIFT_INLINE_THUNK ~UnownedReference() noexcept {
// CHECK:         vwTable->destroy(_getOpaquePointer(), metadata._0);
// CHECK-NEXT:  }

public struct WeakReference {
    weak var value: RefCountedClass?
}

// CHECK-LABEL: SWIFT_INLINE_THUNK ~WeakReference() noexcept {
// CHECK:         vwTable->destroy(_getOpaquePointer(), metadata._0);
// CHECK-NEXT:  }

public struct ArrayMember {
    let values: [Int]
}

// An Array is one bridge object, or one native reference without Objective-C
// interop.
// OFFSET-LABEL: SWIFT_INLINE_THUNK ~ArrayMember() noexcept {
// OFFSET-NEXT:    void *reference;
// OFFSET-NEXT:    memcpy(&reference, _getOpaquePointer(), sizeof(reference));
// OFFSET-NEXT:    swift::_impl::{{swift_bridgeObjectRelease|swift_release}}(reference);
// OFFSET-NEXT:  }

public struct NestedReference {
    let flag: Bool
    let inner: ReferenceAfterTrivial
}

// The offset of a nested reference includes the offsets of its fields.
// OFFSET-LABEL: SWIFT_INLINE_THUNK ~NestedReference() noexcept {
// OFFSET-NEXT:    void *reference;
// OFFSET-NEXT:    memcpy(&reference, _getOpaquePointer() + {{8|16}}, sizeof(reference));
// OFFSET-NEXT:    swift::_impl::swift_release(reference);
// OFFSET-NEXT:  }

public struct ReferenceAfterTrivial {
    let count: Int
    let value: RefCountedClass
}

public func returnNewReferenceAfterTrivial(_ count: Int) -> ReferenceAfterTrivial {
    return ReferenceAfterTrivial(count: count, value: RefCountedClass())
}

public func getCount(_ value: ReferenceAfterTrivial) -> Int {
    value.count
}

// Copies take the trivial bytes along with the reference.
// OFFSET-LABEL: SWIFT_INLINE_THUNK ~ReferenceAfterTrivial() noexcept {
// OFFSET-NEXT:    void *reference;
// OFFSET-NEXT:    memcpy(&reference, _getOpaquePointer() + [[PTR:4|8]], sizeof(reference));
// OFFSET-NEXT:    swift::_impl::swift_release(reference);
// OFFSET-NEXT:  }
// OFFSET-NEXT:  SWIFT_INLINE_THUNK ReferenceAfterTrivial(const ReferenceAfterTrivial &other) noexcept {
// OFFSET-NEXT:    void *reference;
// OFFSET-NEXT:    memcpy(&reference, other._getOpaquePointer() + [[PTR]], sizeof(reference));
// OFFSET-NEXT:    swift::_impl::swift_retain(reference);
// OFFSET-NEXT:    memcpy(_getOpaquePointer(), other._getOpaquePointer(), sizeof(_storage));
// OFFSET-NEXT:  }
// OFFSET-NEXT:  SWIFT_INLINE_THUNK ReferenceAfterTrivial &operator =(const ReferenceAfterTrivial &other) noexcept {
// OFFSET-NEXT:    void *newReference;
// OFFSET-NEXT:    memcpy(&newReference, other._getOpaquePointer() + [[PTR]], sizeof(newReference));
// OFFSET-NEXT:    swift::_impl::swift_retain(newReference);
// OFFSET-NEXT:    void *oldReference;
// OFFSET-NEXT:    memcpy(&oldReference, _getOpaquePointer() + [[PTR]], sizeof(oldReference));
// OFFSET-NEXT:    memmove(_getOpaquePointer(), other._getOpaquePointer(), sizeof(_storage));
// OFFSET-NEXT:    swift::_impl::swift_release(oldReference);
// OFFSET-NEXT:  return *this;
// OFFSET-NEXT:  }
// OFFSET:       class _impl_ReferenceAfterTrivial {
// OFFSET:         static SWIFT_INLINE_THUNK void initializeWithTake(char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// OFFSET-NEXT:      memcpy(destStorage, srcStorage, {{8|16}});
// OFFSET-NEXT:    }

public struct StringMember {
    let value: String
}

// On 64-bit platforms, a String is a trivial word followed by a bridge object.
// STRING-64-LABEL: SWIFT_INLINE_THUNK ~StringMember() noexcept {
// STRING-64-NEXT:    void *reference;
// STRING-64-NEXT:    memcpy(&reference, _getOpaquePointer() + 8, sizeof(reference));
// STRING-64-NEXT:    swift::_impl::swift_bridgeObjectRelease(reference);
// STRING-64-NEXT:  }
// STRING-64-NEXT:  SWIFT_INLINE_THUNK StringMember(const StringMember &other) noexcept {
// STRING-64-NEXT:    void *reference;
// STRING-64-NEXT:    memcpy(&reference, other._getOpaquePointer() + 8, sizeof(reference));
// STRING-64-NEXT:    swift::_impl::swift_bridgeObjectRetain(reference);
// STRING-64-NEXT:    memcpy(_getOpaquePointer(), other._getOpaquePointer(), sizeof(_storage));
// STRING-64-NEXT:  }

// STRING-32-LABEL: SWIFT_INLINE_THUNK ~StringMember() noexcept {
// STRING-32:         vwTable->destroy(_getOpaquePointer(), metadata._0);
// STRING-32-NEXT:  }

public struct TwoReferences {
    let first: RefCountedClass
    let second: RefCountedClass
}

// OFFSET-LABEL: SWIFT_INLINE_THUNK ~TwoReferences() noexcept {
// OFFSET:         vwTable->destroy(_getOpaquePointer(), metadata._0);
// OFFSET-NEXT:  }
