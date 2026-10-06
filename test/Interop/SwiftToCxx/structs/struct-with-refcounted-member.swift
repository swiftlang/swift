// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h
// RUN: %FileCheck %s --check-prefix=FIXED < %t/structs.h
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
// CHECK-NEXT:     memcpy(_getOpaquePointer(), &reference, sizeof(reference));
// CHECK-NEXT:   }
// CHECK-NEXT:   SWIFT_INLINE_THUNK StructWithRefcountedMember &operator =(const StructWithRefcountedMember &other) noexcept {
// CHECK-NEXT:     void *newReference;
// CHECK-NEXT:     memcpy(&newReference, other._getOpaquePointer(), sizeof(newReference));
// CHECK-NEXT:     swift::_impl::swift_retain(newReference);
// CHECK-NEXT:     void *oldReference;
// CHECK-NEXT:     memcpy(&oldReference, _getOpaquePointer(), sizeof(oldReference));
// CHECK-NEXT:     memcpy(_getOpaquePointer(), &newReference, sizeof(newReference));
// CHECK-NEXT:     swift::_impl::swift_release(oldReference);
// CHECK-NEXT:   return *this;
// CHECK-NEXT:   }
// CHECK-NEXT: private:
// CHECK:      class _impl_StructWithRefcountedMember {
// CHECK:        static SWIFT_INLINE_THUNK void initializeWithTake(char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// CHECK-NEXT:     memcpy(destStorage, srcStorage, sizeof(void *));
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

// CHECK-LABEL: SWIFT_INLINE_THUNK ~UnknownReference() noexcept {
// CHECK:         vwTable->destroy(_getOpaquePointer(), metadata._0);
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
