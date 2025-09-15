// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/swift-class-virtual-method-dispatch.swift -module-name Class -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/class.h
// RUN: %FileCheck %s < %t/class.h

// RUN: %check-interop-cxx-header-in-clang(%t/class.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: CPU=arm64e

// note: uses swift-class-virtual-method-dispatch.swift

// CHECK:      void BaseClass::virtualMethod() {
// CHECK-NEXT: void ***selfPtr_ = reinterpret_cast<void ***>( ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT: void **vtable_ = ptrauth_auth_data(*selfPtr_, ptrauth_key_process_independent_data, ptrauth_blend_discriminator(selfPtr_,27361));
// CHECK-NEXT: #else
// CHECK-NEXT: void **vtable_ = *selfPtr_;
// CHECK-NEXT: #endif
// CHECK-NEXT: struct FTypeAddress {
// CHECK-NEXT: decltype(Class::_impl::$s5Class04BaseA0C13virtualMethodyyF) * __ptrauth_swift_class_method_pointer([[#AUTH:]]) func;
// CHECK-NEXT: };
// CHECK-NEXT: FTypeAddress *fptrptr_ = reinterpret_cast<FTypeAddress *>(vtable_ + [[#VM1:]] / sizeof(void *));
// CHECK-NEXT:   (* fptrptr_->func)(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT: }
