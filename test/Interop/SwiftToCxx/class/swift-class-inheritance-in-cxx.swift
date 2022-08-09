// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Class -clang-header-expose-public-decls -emit-clang-header-path %t/class.h
// RUN: %FileCheck %s < %t/class.h

// RUN: %check-interop-cxx-header-in-clang(%t/class.h)

public class BaseClass {
  var field: Int64
    
  init() {
    field = 0
    print("init BaseClass")
  }
  deinit {
    print("destroy BaseClass")
  }
}
public class DerivedClass: BaseClass {
  override init() {
    super.init()
    print("init DerivedClass")
  }
  deinit {
    print("destroy DerivedClass")
  }
}
public final class DerivedDerivedClass: DerivedClass {
  override init() {
    super.init()
    print("init DerivedDerivedClass")
  }
  deinit {
    print("destroy DerivedDerivedClass")
  }
}
public final class SiblingDerivedClass: BaseClass {
}

public func returnDerivedClass() -> DerivedClass {
  return DerivedClass()
}

public func returnDerivedDerivedClass() -> DerivedDerivedClass {
  return DerivedDerivedClass()
}

public func useBaseClass(_ x: BaseClass) {
  print("useBaseClass, type=\(x.self)")
}

public func useDerivedClass(_ x: DerivedClass) {
  print("useDerivedClass, type=\(x.self)")
}

// CHECK:      class DerivedClass : public BaseClass {
// CHECK-NEXT:  public:
// CHECK-NEXT:    using BaseClass::BaseClass;
// CHECK-NEXT:    using BaseClass::operator=;
// CHECK-NEXT:  protected:
// CHECK-NEXT:    inline DerivedClass(void * _Nonnull ptr) noexcept : BaseClass(ptr) {}
// CHECK-NEXT:  private:
// CHECK-NEXT:    friend class _impl::_impl_DerivedClass;
// CHECK-NEXT:  };

// CHECK:      class DerivedDerivedClass final : public DerivedClass {
// CHECK-NEXT: public:
// CHECK-NEXT:   using DerivedClass::DerivedClass;
// CHECK-NEXT:   using DerivedClass::operator=;
// CHECK-NEXT: protected:
// CHECK-NEXT:   inline DerivedDerivedClass(void * _Nonnull ptr) noexcept : DerivedClass(ptr) {}
// CHECK-NEXT: private:
// CHECK-NEXT:   friend class _impl::_impl_DerivedDerivedClass;
// CHECK-NEXT: };

// Verify base class names are adjusted to avoid conflict with C++ keywords.
public class auto {}
public class derivedRegister: auto {}

// CHECK:      class derivedRegister : public auto_ {
// CHECK:        using auto_::auto_;
// CHECK:        using auto_::operator=;
// CHECK:        inline derivedRegister(void * _Nonnull ptr) noexcept : auto_(ptr) {}
