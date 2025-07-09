// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/structs.h -Wno-unused-function -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

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

// CHECK:      class SWIFT_SYMBOL({{.*}}) StructWithRefcountedMember final {
// CHECK-NEXT: public:
// CHECK-NEXT:   SWIFT_INLINE_THUNK ~StructWithRefcountedMember() noexcept {
// CHECK-NEXT:     auto metadata = _impl::$s7Structs26StructWithRefcountedMemberVMa(0);
// CHECK-NEXT:     auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:     vwTable->destroy(_getOpaquePointer(), metadata._0);
// CHECK-NEXT:   }
// CHECK-NEXT:   SWIFT_INLINE_THUNK StructWithRefcountedMember(const StructWithRefcountedMember &other) noexcept {
// CHECK-NEXT:     auto metadata = _impl::$s7Structs26StructWithRefcountedMemberVMa(0);
// CHECK-NEXT:     auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:     vwTable->initializeWithCopy(_getOpaquePointer(), const_cast<char *>(other._getOpaquePointer()), metadata._0);
// CHECK-NEXT:   }
// CHECK-NEXT:   SWIFT_INLINE_THUNK StructWithRefcountedMember &operator =(const StructWithRefcountedMember &other) noexcept {
// CHECK-NEXT:     auto metadata = _impl::$s7Structs26StructWithRefcountedMemberVMa(0);
// CHECK-NEXT:     auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT:   #ifdef __arm64e__
// CHECK-NEXT:     auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT:   #else
// CHECK-NEXT:     auto *vwTable = *vwTableAddr;
// CHECK-NEXT:   #endif
// CHECK-NEXT:     vwTable->assignWithCopy(_getOpaquePointer(), const_cast<char *>(other._getOpaquePointer()), metadata._0);
// CHECK-NEXT:   return *this;
// CHECK-NEXT:   }
// CHECK-NEXT: private:
