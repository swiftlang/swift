// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Class -clang-header-expose-public-decls -emit-clang-header-path %t/class.h
// RUN: %FileCheck %s < %t/class.h

// RUN: %check-interop-cxx-header-in-clang(%t/class.h)

public final class ClassWithIntField {
  var field: Int64

  init() {
    field = 0
    print("init ClassWithIntField")
  }
  deinit {
    print("destroy ClassWithIntField")
  }
}

// CHECK: namespace Class {

// CHECK: SWIFT_EXTERN void * _Nonnull $s5Class011passThroughA12WithIntFieldyAA0adeF0CADF(void * _Nonnull x) SWIFT_NOEXCEPT SWIFT_CALL; // passThroughClassWithIntField(_:)
// CHECK-NEXT: SWIFT_EXTERN void * _Nonnull $s5Class06returnA12WithIntFieldAA0acdE0CyF(void) SWIFT_NOEXCEPT SWIFT_CALL; // returnClassWithIntField()
// CHECK-NEXT: SWIFT_EXTERN void $s5Class04takeA12WithIntFieldyyAA0acdE0CF(void * _Nonnull x) SWIFT_NOEXCEPT SWIFT_CALL; // takeClassWithIntField(_:)
// CHECK-NEXT: SWIFT_EXTERN void $s5Class04takeA17WithIntFieldInoutyyAA0acdE0CzF(void * _Nonnull * _Nonnull x) SWIFT_NOEXCEPT SWIFT_CALL; // takeClassWithIntFieldInout(_:)

// CHECK: namespace Class {

// CHECK: namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT: class _impl_ClassWithIntField;
// CHECK-EMPTY:
// CHECK-NEXT: } // namespace _impl
// CHECK-EMPTY:
// CHECK-NEXT: class ClassWithIntField final : swift::_impl::RefCountedClass {
// CHECK-NEXT: public:
// CHECK-NEXT:   using RefCountedClass::RefCountedClass;
// CHECK-NEXT:   using RefCountedClass::operator=;
// CHECK-NEXT: private:
// CHECK-NEXT:   inline ClassWithIntField(void * _Nonnull ptr) noexcept : RefCountedClass(ptr) {}
// CHECK-EMPTY:
// CHECK-NEXT:   friend class _impl::_impl_ClassWithIntField;
// CHECK-NEXT: };
// CHECK-EMPTY:
// CHECK-NEXT:namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT:class _impl_ClassWithIntField {
// CHECK-NEXT:public:
// CHECK-NEXT:static inline ClassWithIntField makeRetained(void * _Nonnull ptr) noexcept { return ClassWithIntField(ptr); }
// CHECK-NEXT:static inline void * _Nonnull getOpaquePointer(const ClassWithIntField &object) noexcept { return object._opaquePointer; }
// CHECK-NEXT:static inline void * _Nonnull &getOpaquePointerRef(ClassWithIntField &object) noexcept { return object._opaquePointer; }
// CHECK-NEXT:};
// CHECK-EMPTY:
// CHECK-NEXT:} // namespace _impl

// CHECK: inline ClassWithIntField passThroughClassWithIntField(const ClassWithIntField& x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::_impl_ClassWithIntField::makeRetained(_impl::$s5Class011passThroughA12WithIntFieldyAA0adeF0CADF(_impl::_impl_ClassWithIntField::getOpaquePointer(x)));
// CHECK-NEXT: }

public final class register { }

// CHECK: class register_ final : swift::_impl::RefCountedClass {

public func returnClassWithIntField() -> ClassWithIntField {
  return ClassWithIntField()
}


public func passThroughClassWithIntField(_ x: ClassWithIntField) -> ClassWithIntField {
  x.field = 42
  return x
}

public func takeClassWithIntField(_ x: ClassWithIntField) {
  print("ClassWithIntField: \(x.field);")
}

public func takeClassWithIntFieldInout(_ x: inout ClassWithIntField) {
  x = ClassWithIntField()
  x.field = -11
}

// CHECK: inline ClassWithIntField returnClassWithIntField() noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::_impl_ClassWithIntField::makeRetained(_impl::$s5Class06returnA12WithIntFieldAA0acdE0CyF());
// CHECK-NEXT: }

// CHECK: inline void takeClassWithIntField(const ClassWithIntField& x) noexcept {
// CHECK-NEXT:  return _impl::$s5Class04takeA12WithIntFieldyyAA0acdE0CF(_impl::_impl_ClassWithIntField::getOpaquePointer(x));
// CHECK-NEXT: }
