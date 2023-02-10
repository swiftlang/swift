// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Class -clang-header-expose-decls=all-public -emit-clang-header-path %t/class.h
// RUN: %FileCheck %s < %t/class.h

// RUN: %check-interop-cxx-header-in-clang(%t/class.h)

public class BaseClass {
  var field: Int64
    
  init() {
    field = 0
  }
  public func virtualMethod() {
    print("BaseClass.virtualMethod")
  }
  public func virtualMethodIntInt(_ x: Int) -> Int {
    return x
  }
  public final func finalMethodInBase(_ x: Int) -> Int {
    return x * 2
  }
}

public class DerivedClass: BaseClass {
  override init() {
    super.init()
  }
  override public func virtualMethod() {
    print("DerivedClass.virtualMethod")
  }
  override public func virtualMethodIntInt(_ x: Int) -> Int {
    return -x
  }
  public func virtualMethodInDerived(_ x: BaseClass) -> BaseClass {
    return x
  }
}

public final class DerivedDerivedClass: DerivedClass {
  override init() {
    super.init()
  }
  override public func virtualMethod() {
    print("DerivedDerivedClass.virtualMethod")
  }
  override public func virtualMethodInDerived(_ x: BaseClass) -> BaseClass {
    return self
  }
  public func methodInDerivedDerived() {
    print("DerivedDerivedClass.methodInDerivedDerived")
  }
}

public func returnBaseClass() -> BaseClass {
  return BaseClass()
}

public func returnDerivedClass() -> DerivedClass {
  return DerivedClass()
}

public func returnDerivedDerivedClass() -> DerivedDerivedClass {
  return DerivedDerivedClass()
}

// CHECK:      void BaseClass::virtualMethod() {
// CHECK-NEXT: void ***selfPtr_ = reinterpret_cast<void ***>( ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT: void **vtable_ = *selfPtr_;
// CHECK-NEXT: using FType = decltype(_impl::$s5Class04BaseA0C13virtualMethodyyF);
// CHECK-NEXT: FType *fptr_ = reinterpret_cast<FType *>(*(vtable_ + [[#VM1:]]));
// CHECK-NEXT:   return (* fptr_)(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:  }

// CHECK:      swift::Int BaseClass::virtualMethodIntInt(swift::Int x) {
// CHECK-NEXT: void ***selfPtr_ = reinterpret_cast<void ***>( ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT: void **vtable_ = *selfPtr_;
// CHECK-NEXT: using FType = decltype(_impl::$s5Class04BaseA0C016virtualMethodIntE0yS2iF);
// CHECK-NEXT: FType *fptr_ = reinterpret_cast<FType *>(*(vtable_ + [[#VM1 + 1]]));
// CHECK-NEXT:   return (* fptr_)(x, ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:   }

// CHECK:        swift::Int BaseClass::finalMethodInBase(swift::Int x) {
// CHECK-NEXT:   return _impl::$s5Class04BaseA0C013finalMethodInB0yS2iF(x, ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:   }

// CHECK:        void DerivedClass::virtualMethod() {
// CHECK-NEXT:   void ***selfPtr_ = reinterpret_cast<void ***>( ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:   void **vtable_ = *selfPtr_;
// CHECK-NEXT:   using FType = decltype(_impl::$s5Class07DerivedA0C13virtualMethodyyF);
// CHECK-NEXT:   FType *fptr_ = reinterpret_cast<FType *>(*(vtable_ + [[#VM1]]));
// CHECK-NEXT:     return (* fptr_)(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:     }

// CHECK:        swift::Int DerivedClass::virtualMethodIntInt(swift::Int x) {
// CHECK-NEXT:   void ***selfPtr_ = reinterpret_cast<void ***>( ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:   void **vtable_ = *selfPtr_;
// CHECK-NEXT:   using FType = decltype(_impl::$s5Class07DerivedA0C016virtualMethodIntE0yS2iF);
// CHECK-NEXT:   FType *fptr_ = reinterpret_cast<FType *>(*(vtable_ + [[#VM1 + 1]]));
// CHECK-NEXT:     return (* fptr_)(x, ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:     }

// CHECK:        BaseClass DerivedClass::virtualMethodInDerived(const BaseClass& x) {
// CHECK-NEXT:   void ***selfPtr_ = reinterpret_cast<void ***>( ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:   void **vtable_ = *selfPtr_;
// CHECK-NEXT:   using FType = decltype(_impl::$s5Class07DerivedA0C015virtualMethodInB0yAA04BaseA0CAFF);
// CHECK-NEXT:   FType *fptr_ = reinterpret_cast<FType *>(*(vtable_ + [[#VM1 + 2]]));
// CHECK-NEXT:     return _impl::_impl_BaseClass::makeRetained((* fptr_)(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(x), ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this)));
// CHECK-NEXT:     }

// CHECK:        void DerivedDerivedClass::virtualMethod() {
// CHECK-NEXT:     return _impl::$s5Class07DerivedbA0C13virtualMethodyyF(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:     }

// CHECK:        BaseClass DerivedDerivedClass::virtualMethodInDerived(const BaseClass& x) {
// CHECK-NEXT:     return _impl::_impl_BaseClass::makeRetained(_impl::$s5Class07DerivedbA0C015virtualMethodInB0yAA04BaseA0CAFF(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(x), ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this)));
// CHECK-NEXT:     }

// CHECK:        void DerivedDerivedClass::methodInDerivedDerived() {
// CHECK-NEXT:     return _impl::$s5Class07DerivedbA0C08methodInbB0yyF(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT:     }

