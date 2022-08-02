// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Class -clang-header-expose-public-decls -emit-clang-header-path %t/class.h
// RUN: %FileCheck %s < %t/class.h

// RUN: %check-interop-cxx-header-in-clang(%t/class.h)

public final class ClassWithIntField {
  let field: Int64

  init() {
    field = 0
    print("init ClassWithIntField")
  }
  deinit {
    print("destroy ClassWithIntField")
  }
}

// CHECK: namespace Class {

// CHECK: SWIFT_EXTERN void * _Nonnull $s5Class06returnA12WithIntFieldAA0acdE0CyF(void) SWIFT_NOEXCEPT SWIFT_CALL; // returnClassWithIntField()

// CHECK: namespace Class {

// CHECK: namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT: class _impl_ClassWithIntField;
// CHECK-EMPTY:
// CHECK-NEXT: } // namespace _impl
// CHECK-EMPTY:
// CHECK-NEXT: class ClassWithIntField final {
// CHECK-NEXT: public:
// CHECK-NEXT:   inline ~ClassWithIntField() { swift::_impl::swift_release(_opaquePointer); }
// CHECK-NEXT:   inline ClassWithIntField(ClassWithIntField&&) noexcept = default;
// CHECK-NEXT: private:
// CHECK-NEXT:   inline ClassWithIntField(void * _Nonnull ptr) noexcept : _opaquePointer(ptr) {}
// CHECK-EMPTY:
// CHECK-NEXT:   void * _Nonnull _opaquePointer;
// CHECK-NEXT:   friend class _impl::_impl_ClassWithIntField;
// CHECK-NEXT: };
// CHECK-EMPTY:
// CHECK-NEXT:namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT:class _impl_ClassWithIntField {
// CHECK-NEXT:public:
// CHECK-NEXT:static inline ClassWithIntField fromUnretained(void * _Nonnull ptr) noexcept { return ClassWithIntField(ptr); }
// CHECK-NEXT:};
// CHECK-EMPTY:
// CHECK-NEXT:} // namespace _impl

public final class register { }

// CHECK: class register_ final {

public func returnClassWithIntField() -> ClassWithIntField {
  return ClassWithIntField()
}

// CHECK: inline ClassWithIntField returnClassWithIntField() noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::_impl_ClassWithIntField::fromUnretained(_impl::$s5Class06returnA12WithIntFieldAA0acdE0CyF());
// CHECK-NEXT: }
