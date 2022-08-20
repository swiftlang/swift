// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Methods -clang-header-expose-public-decls -emit-clang-header-path %t/methods.h
// RUN: %FileCheck %s < %t/methods.h

// RUN: %check-interop-cxx-header-in-clang(%t/methods.h)

public struct LargeStruct {
    let x1, x2, x3, x4, x5, x6: Int

    public func doubled() -> LargeStruct {
        return LargeStruct(x1: x1 * 2, x2: x2 * 2, x3: x3 * 2, x4: x4 * 2, x5: x5 * 2, x6: x6 * 2)
    }

    public func dump() {
        print("\(x1), \(x2), \(x3), \(x4), \(x5), \(x6)")
    }

    public func scaled(_ x: Int, _ y: Int) -> LargeStruct {
        return LargeStruct(x1: x1 * x, x2: x2 * y, x3: x3 * x, x4: x4 * y, x5: x5 * x, x6: x6 * y)
    }

    public func added(_ x: LargeStruct) -> LargeStruct {
        return LargeStruct(x1: x1 + x.x1, x2: x2 + x.x2, x3: x3 + x.x3, x4: x4 + x.x4, x5: x5 + x.x5, x6: x6 + x.x6)
    }
}

public final class ClassWithMethods {
    var field: Int

    init(_ x: Int) {
      field = x
    }

    deinit {
        print("ClassWithMethods \(field) deinit")
    }

    public func dump() {
        print("ClassWithMethods \(field);")
    }

    public func sameRet() -> ClassWithMethods {
        return self
    }

    public func mutate() {
        field = -field
    }

    public func deepCopy(_ x: Int) -> ClassWithMethods {
        return ClassWithMethods(field + x)
    }
}

public final class PassStructInClassMethod {
    var largeStruct: LargeStruct
    init() { largeStruct = LargeStruct(x1: 1, x2: 2, x3: 3, x4: 4, x5: 5, x6: 6) }

    public func retStruct(_ x: Int) -> LargeStruct {
        print("PassStructInClassMethod.retStruct \(x);")
        return largeStruct
    }
    public func updateStruct(_ x: Int, _ y: LargeStruct) {
        largeStruct = LargeStruct(x1: x, x2: y.x2, x3: y.x3, x4: y.x4, x5: y.x5, x6: y.x6)
    }
}

// CHECK: SWIFT_EXTERN void $s7Methods09ClassWithA0C4dumpyyF(SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // dump()
// CHECK: SWIFT_EXTERN void * _Nonnull $s7Methods09ClassWithA0C7sameRetACyF(SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // sameRet()
// CHECK: SWIFT_EXTERN void $s7Methods09ClassWithA0C6mutateyyF(SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // mutate()
// CHECK: SWIFT_EXTERN void * _Nonnull $s7Methods09ClassWithA0C8deepCopyyACSiF(ptrdiff_t x, SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // deepCopy(_:)
// CHECK: SWIFT_EXTERN void $s7Methods11LargeStructV7doubledACyF(SWIFT_INDIRECT_RESULT void * _Nonnull, SWIFT_CONTEXT const void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // doubled()
// CHECK:  SWIFT_EXTERN void $s7Methods11LargeStructV4dumpyyF(SWIFT_CONTEXT const void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // dump()
// CHECK:  SWIFT_EXTERN void $s7Methods11LargeStructV6scaledyACSi_SitF(SWIFT_INDIRECT_RESULT void * _Nonnull, ptrdiff_t x, ptrdiff_t y, SWIFT_CONTEXT const void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // scaled(_:_:)
// CHECK:  SWIFT_EXTERN void $s7Methods11LargeStructV5addedyA2CF(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull x, SWIFT_CONTEXT const void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // added(_:)
// CHECK: SWIFT_EXTERN void $s7Methods23PassStructInClassMethodC03retC0yAA05LargeC0VSiF(SWIFT_INDIRECT_RESULT void * _Nonnull, ptrdiff_t x, SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // retStruct(_:)
// CHECK: SWIFT_EXTERN void $s7Methods23PassStructInClassMethodC06updateC0yySi_AA05LargeC0VtF(ptrdiff_t x, const void * _Nonnull y, SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // updateStruct(_:_:)

// CHECK: class ClassWithMethods final : public swift::_impl::RefCountedClass {
// CHECK:   using RefCountedClass::RefCountedClass;
// CHECK-NEXT:   using RefCountedClass::operator=;
// CHECK-NEXT:   inline void dump();
// CHECK-NEXT:   inline ClassWithMethods sameRet();
// CHECK-NEXT:   inline void mutate();
// CHECK-NEXT:   inline ClassWithMethods deepCopy(swift::Int x);

// CHECK: class LargeStruct final {
// CHECK: inline LargeStruct(LargeStruct &&) = default;
// CHECK-NEXT: inline LargeStruct doubled() const;
// CHECK-NEXT: inline void dump() const;
// CHECK-NEXT: inline LargeStruct scaled(swift::Int x, swift::Int y) const;
// CHECK-NEXT: inline LargeStruct added(const LargeStruct& x) const;
// CHECK-NEXT: private

public func createClassWithMethods(_ x: Int) -> ClassWithMethods {
    return ClassWithMethods(x)
}

public func createLargeStruct() -> LargeStruct {
    return LargeStruct(x1: -1, x2: 2, x3: -100, x4: 42, x5: 67, x6: -10101)
}

public func createPassStructInClassMethod() -> PassStructInClassMethod {
    return PassStructInClassMethod()
}


// CHECK: inline void ClassWithMethods::dump() {
// CHECK-NEXT: return _impl::$s7Methods09ClassWithA0C4dumpyyF(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT: }
// CHECK-NEXT: inline ClassWithMethods ClassWithMethods::sameRet() {
// CHECK-NEXT: return _impl::_impl_ClassWithMethods::makeRetained(_impl::$s7Methods09ClassWithA0C7sameRetACyF(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this)));
// CHECK-NEXT: }
// CHECK-NEXT: inline void ClassWithMethods::mutate() {
// CHECK-NEXT: return _impl::$s7Methods09ClassWithA0C6mutateyyF(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT: }
// CHECK-NEXT: inline ClassWithMethods ClassWithMethods::deepCopy(swift::Int x) {
// CHECK-NEXT: return _impl::_impl_ClassWithMethods::makeRetained(_impl::$s7Methods09ClassWithA0C8deepCopyyACSiF(x, ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this)));
// CHECK-NEXT: }

// CHECK:      inline LargeStruct LargeStruct::doubled() const {
// CHECK-NEXT: return _impl::_impl_LargeStruct::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:   _impl::$s7Methods11LargeStructV7doubledACyF(result, _getOpaquePointer());
// CHECK-NEXT: });
// CHECK-NEXT: }
// CHECK-NEXT: inline void LargeStruct::dump() const {
// CHECK-NEXT: return _impl::$s7Methods11LargeStructV4dumpyyF(_getOpaquePointer());
// CHECK-NEXT: }
// CHECK-NEXT: inline LargeStruct LargeStruct::scaled(swift::Int x, swift::Int y) const {
// CHECK-NEXT: return _impl::_impl_LargeStruct::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:   _impl::$s7Methods11LargeStructV6scaledyACSi_SitF(result, x, y, _getOpaquePointer());
// CHECK-NEXT: });
// CHECK-NEXT: }
// CHECK-NEXT: inline LargeStruct LargeStruct::added(const LargeStruct& x) const {
// CHECK-NEXT: return _impl::_impl_LargeStruct::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:   _impl::$s7Methods11LargeStructV5addedyA2CF(result, _impl::_impl_LargeStruct::getOpaquePointer(x), _getOpaquePointer());
// CHECK-NEXT: });
// CHECK-NEXT: }

// CHECK: inline LargeStruct PassStructInClassMethod::retStruct(swift::Int x) {
// CHECK-NEXT: return _impl::_impl_LargeStruct::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:   _impl::$s7Methods23PassStructInClassMethodC03retC0yAA05LargeC0VSiF(result, x, ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT: });
// CHECK-NEXT: }
// CHECK-NEXT: inline void PassStructInClassMethod::updateStruct(swift::Int x, const LargeStruct& y) {
// CHECK-NEXT: return _impl::$s7Methods23PassStructInClassMethodC06updateC0yySi_AA05LargeC0VtF(x, _impl::_impl_LargeStruct::getOpaquePointer(y), ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT: }
