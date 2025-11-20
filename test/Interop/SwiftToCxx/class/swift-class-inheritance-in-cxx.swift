// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Class -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/class.h
// RUN: %FileCheck %s < %t/class.h

// RUN: %check-interop-cxx-header-in-clang(%t/class.h)

// RUN: %target-swift-frontend %s -module-name Class -enable-library-evolution -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/class-evo.h
// RUN: %FileCheck %s < %t/class-evo.h

// RUN: %check-interop-cxx-header-in-clang(%t/class-evo.h)

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

// CHECK:      class SWIFT_SYMBOL("s:5Class07DerivedA0C") DerivedClass : public BaseClass {
// CHECK-NEXT:  public:
// CHECK-NEXT:    using BaseClass::BaseClass;
// CHECK-NEXT:    using BaseClass::operator=;
// CHECK-NEXT:  protected:
// CHECK-NEXT:    SWIFT_INLINE_THUNK DerivedClass(void * _Nonnull ptr) noexcept : BaseClass(ptr) {}
// CHECK-NEXT:  private:
// CHECK-NEXT:    friend class _impl::_impl_DerivedClass;
// CHECK-NEXT: #pragma clang diagnostic push
// CHECK-NEXT: #pragma clang diagnostic ignored "-Wc++17-extensions"
// CHECK-NEXT: #pragma clang diagnostic push
// CHECK-NEXT: #pragma clang diagnostic ignored "-Wreserved-identifier"
// CHECK-NEXT:   typedef char $s5Class07DerivedA0CD;
// CHECK-NEXT:   static inline constexpr $s5Class07DerivedA0CD __swift_mangled_name = 0;
// CHECK-NEXT: #pragma clang diagnostic pop
// CHECK-NEXT: #pragma clang diagnostic pop
// CHECK-NEXT:  };

// CHECK:      class SWIFT_SYMBOL("s:5Class07DerivedbA0C") DerivedDerivedClass final : public DerivedClass {
// CHECK-NEXT: public:
// CHECK-NEXT:   using DerivedClass::DerivedClass;
// CHECK-NEXT:   using DerivedClass::operator=;
// CHECK-NEXT: protected:
// CHECK-NEXT:   SWIFT_INLINE_THUNK DerivedDerivedClass(void * _Nonnull ptr) noexcept : DerivedClass(ptr) {}
// CHECK-NEXT: private:
// CHECK-NEXT:   friend class _impl::_impl_DerivedDerivedClass;
// CHECK-NEXT: #pragma clang diagnostic push
// CHECK-NEXT: #pragma clang diagnostic ignored "-Wc++17-extensions"
// CHECK-NEXT: #pragma clang diagnostic push
// CHECK-NEXT: #pragma clang diagnostic ignored "-Wreserved-identifier"
// CHECK-NEXT:   typedef char $s5Class07DerivedbA0CD;
// CHECK-NEXT:   static inline constexpr $s5Class07DerivedbA0CD __swift_mangled_name = 0;
// CHECK-NEXT: #pragma clang diagnostic pop
// CHECK-NEXT: #pragma clang diagnostic pop
// CHECK-NEXT: };

// Verify base class names are adjusted to avoid conflict with C++ keywords.
public class auto {}
public class derivedRegister: auto {}

// CHECK:      class SWIFT_SYMBOL("s:5Class15derivedRegisterC") derivedRegister : public auto_ {
// CHECK:        using auto_::auto_;
// CHECK:        using auto_::operator=;
// CHECK:        SWIFT_INLINE_THUNK derivedRegister(void * _Nonnull ptr) noexcept : auto_(ptr) {}
