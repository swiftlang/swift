// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/swift-class-virtual-method-dispatch.swift -typecheck -module-name Class -clang-header-expose-decls=all-public -emit-clang-header-path %t/class.h -enable-library-evolution
// RUN: %FileCheck %s < %t/class.h

// RUN: %check-interop-cxx-header-in-clang(%t/class.h)

// note: implemented in swift-class-virtual-method-dispatch.swift

// CHECK:      void BaseClass::virtualMethod() {
// CHECK-NEXT: return _impl::$s5Class04BaseA0C13virtualMethodyyFTj(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:  }

// CHECK:      swift::Int BaseClass::virtualMethodIntInt(swift::Int x) {
// CHECK-NEXT: return _impl::$s5Class04BaseA0C016virtualMethodIntE0yS2iFTj(x, ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:   }

// CHECK:        swift::Int BaseClass::finalMethodInBase(swift::Int x) {
// CHECK-NEXT:   return _impl::$s5Class04BaseA0C013finalMethodInB0yS2iF(x, ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:   }

// CHECK:        void DerivedClass::virtualMethod() {
// CHECK-NEXT:   return _impl::$s5Class04BaseA0C13virtualMethodyyFTj(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:   }

// CHECK:        swift::Int DerivedClass::virtualMethodIntInt(swift::Int x) {
// CHECK-NEXT:   return _impl::$s5Class04BaseA0C016virtualMethodIntE0yS2iFTj(x, ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:   }

// CHECK:        BaseClass DerivedClass::virtualMethodInDerived(const BaseClass& x) {
// CHECK-NEXT:   return _impl::_impl_BaseClass::makeRetained(_impl::$s5Class07DerivedA0C015virtualMethodInB0yAA04BaseA0CAFFTj(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(x), ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this)));
// CHECK-NEXT:   }

// CHECK:        void DerivedDerivedClass::virtualMethod() {
// CHECK-NEXT:     return _impl::$s5Class07DerivedbA0C13virtualMethodyyF(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:     }

// CHECK:        BaseClass DerivedDerivedClass::virtualMethodInDerived(const BaseClass& x) {
// CHECK-NEXT:     return _impl::_impl_BaseClass::makeRetained(_impl::$s5Class07DerivedbA0C015virtualMethodInB0yAA04BaseA0CAFF(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(x), ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this)));
// CHECK-NEXT:     }

// CHECK:        void DerivedDerivedClass::methodInDerivedDerived() {
// CHECK-NEXT:     return _impl::$s5Class07DerivedbA0C08methodInbB0yyF(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:     }

