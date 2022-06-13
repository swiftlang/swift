// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Structs -clang-header-expose-public-decls -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/structs.h -Wno-unused-function)

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

// CHECK:      class StructWithRefcountedMember final {
// CHECK-NEXT: public:
// CHECK-NEXT:   inline ~StructWithRefcountedMember() {
// CHECK-NEXT:     auto metadata = _impl::$s7Structs26StructWithRefcountedMemberVMa(0);
// CHECK-NEXT:     auto *vwTable = *(reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1);
// CHECK-NEXT:     vwTable->destroy(_getOpaquePointer(), metadata._0);
// CHECK-NEXT:   }
// CHECK-NEXT:   inline StructWithRefcountedMember(const StructWithRefcountedMember &) = default;
// CHECK-NEXT:   inline StructWithRefcountedMember(StructWithRefcountedMember &&) = default;
// CHECK-NEXT: private:
