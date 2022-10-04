// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Init -clang-header-expose-decls=all-public -emit-clang-header-path %t/inits.h
// RUN: %FileCheck %s < %t/inits.h

// RUN: %check-interop-cxx-header-in-clang(%t/inits.h -Wno-unused-function)

// RUN: %target-swift-frontend %s -typecheck -module-name Init -clang-header-expose-decls=all-public -swift-version 5 -emit-clang-header-path %t/inits2.h
// RUN: %FileCheck %s < %t/inits2.h


// CHECK: SWIFT_EXTERN void * _Nonnull $s4Init9BaseClassCyACSi_SitcfC(ptrdiff_t x, ptrdiff_t y, SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // init(_:_:)
// CHECK-NEXT: SWIFT_EXTERN void * _Nonnull $s4Init9BaseClassCyACSicfC(ptrdiff_t x, SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // init(_:)
// CHECK-NEXT: SWIFT_EXTERN void * _Nonnull $s4Init12DerivedClassCyACSi_SitcfC(ptrdiff_t x, ptrdiff_t y, SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // init(_:_:)

// CHECK:      SWIFT_EXTERN struct swift_interop_returnStub_Init_uint32_t_0_4 $s4Init16FirstSmallStructVACycfC(void) SWIFT_NOEXCEPT SWIFT_CALL; // init()
// CHECK-NEXT: SWIFT_EXTERN struct swift_interop_returnStub_Init_uint32_t_0_4 $s4Init16FirstSmallStructVyACSicfC(ptrdiff_t x) SWIFT_NOEXCEPT SWIFT_CALL; // init(_:)

// CHECK:      SWIFT_EXTERN void $s4Init11LargeStructVACycfC(SWIFT_INDIRECT_RESULT void * _Nonnull) SWIFT_NOEXCEPT SWIFT_CALL; // init()
// CHECK-NEXT: SWIFT_EXTERN void $s4Init11LargeStructV1x1yACSi_AA010FirstSmallC0VtcfC(SWIFT_INDIRECT_RESULT void * _Nonnull, ptrdiff_t x, struct swift_interop_passStub_Init_uint32_t_0_4 y) SWIFT_NOEXCEPT SWIFT_CALL; // init(x:y:)

// CHECK:      SWIFT_EXTERN struct swift_interop_returnStub_Init_[[PTRENC:void_ptr_0_[0-9]]] $s4Init28StructWithRefCountStoredPropVACycfC(void) SWIFT_NOEXCEPT SWIFT_CALL; // init()
// CHECK-NEXT: SWIFT_EXTERN struct swift_interop_returnStub_Init_[[PTRENC]] $s4Init28StructWithRefCountStoredPropV1xACSi_tcfC(ptrdiff_t x) SWIFT_NOEXCEPT SWIFT_CALL; // init(x:)

public struct FirstSmallStruct {
    public let x: UInt32
    
    public init() {
        x = 42
    }

    public init(_ x: Int) {
        self.x = UInt32(x)
    }

    private init(_ y: Float) {
        fatalError("nope")
    }
}

// CHECK: class FirstSmallStruct final {
// CHECK-NEXT: public:
// CHECK:   inline FirstSmallStruct(FirstSmallStruct &&)
// CHECK-NEXT:   inline uint32_t getX() const;
// CHECK-NEXT:   static inline FirstSmallStruct init();
// CHECK-NEXT:   static inline FirstSmallStruct init(swift::Int x);
// CHECK-NEXT: private:

public struct LargeStruct {
    public let x1, x2, x3, x4, x5, x6: Int

    public init () {
        x1 = 0
        x2 = 0
        x3 = 0
        x4 = 0
        x5 = 0
        x6 = 0
    }

    public init(x: Int, y: FirstSmallStruct) {
        x1 = x
        x2 = Int(y.x)
        x3 = -x
        x4 = 0
        x5 = 11
        x6 = x * Int(y.x)
    }
}

// CHECK: class LargeStruct final {
// CHECK:       inline swift::Int getX6() const;
// CHECK-NEXT:  static inline LargeStruct init();
// CHECK-NEXT:  static inline LargeStruct init(swift::Int x, const FirstSmallStruct& y);
// CHECK-NEXT: private:

private class RefCountedClass {
    let x: Int

    init(x: Int) {
        self.x = x
        print("create RefCountedClass \(x)")
    }
    deinit {
        print("destroy RefCountedClass \(x)")
    }
}

public struct StructWithRefCountStoredProp {
    private let storedRef: RefCountedClass

    public init() {
        storedRef = RefCountedClass(x: -1)
    }

    public init(x: Int) {
        storedRef = RefCountedClass(x: x)
    }
}

// CHECK:      static inline StructWithRefCountStoredProp init();
// CHECK-NEXT: static inline StructWithRefCountStoredProp init(swift::Int x);


public final class FinalClass {
    public var prop: FirstSmallStruct

    public init(_ prop: FirstSmallStruct) { self.prop = prop }
}

public class BaseClass {
    public var x: Int

    public init(_ x: Int, _ y: Int) { self.x = x + y }

    public convenience init(_ x: Int) {
        self.init(x, x * 2)
    }
}

public class DerivedClass: BaseClass {
    override public init(_ x: Int, _ y: Int) {
        super.init(x + 1, y + 1)
    }
}

public class DerivedClassTwo: BaseClass {
}

// CHECK: BaseClass BaseClass::init(swift::Int x, swift::Int y) {
// CHECK-NEXT: return _impl::_impl_BaseClass::makeRetained(_impl::$s4Init9BaseClassCyACSi_SitcfC(x, y, swift::TypeMetadataTrait<BaseClass>::getTypeMetadata()));

// CHECK: DerivedClass DerivedClass::init(swift::Int x, swift::Int y) {
// CHECK-NEXT: _impl::_impl_DerivedClass::makeRetained(_impl::$s4Init12DerivedClassCyACSi_SitcfC(x, y, swift::TypeMetadataTrait<DerivedClass>::getTypeMetadata()));

// CHECK: DerivedClassTwo DerivedClassTwo::init(swift::Int x, swift::Int y) {
// CHECK-NEXT: return _impl::_impl_DerivedClassTwo::makeRetained(_impl::$s4Init15DerivedClassTwoCyACSi_SitcfC(x, y, swift::TypeMetadataTrait<DerivedClassTwo>::getTypeMetadata()));

// CHECK: FinalClass FinalClass::init(const FirstSmallStruct& prop) {
// CHECK-NEXT: return _impl::_impl_FinalClass::makeRetained(_impl::$s4Init10FinalClassCyAcA16FirstSmallStructVcfC(_impl::swift_interop_passDirect_Init_uint32_t_0_4(_impl::_impl_FirstSmallStruct::getOpaquePointer(prop)), swift::TypeMetadataTrait<FinalClass>::getTypeMetadata()));


// CHECK:      inline uint32_t FirstSmallStruct::getX() const {
// CHECK-NEXT: return _impl::$s4Init16FirstSmallStructV1xs6UInt32Vvg(_impl::swift_interop_passDirect_Init_uint32_t_0_4(_getOpaquePointer()));
// CHECK-NEXT: }
// CHECK-NEXT: inline FirstSmallStruct FirstSmallStruct::init() {
// CHECK-NEXT: return _impl::_impl_FirstSmallStruct::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:   _impl::swift_interop_returnDirect_Init_uint32_t_0_4(result, _impl::$s4Init16FirstSmallStructVACycfC());
// CHECK-NEXT: });
// CHECK-NEXT: }
// CHECK-NEXT: inline FirstSmallStruct FirstSmallStruct::init(swift::Int x) {
// CHECK-NEXT: return _impl::_impl_FirstSmallStruct::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:   _impl::swift_interop_returnDirect_Init_uint32_t_0_4(result, _impl::$s4Init16FirstSmallStructVyACSicfC(x));
// CHECK-NEXT: });
// CHECK-NEXT: }

// CHECK:      inline LargeStruct LargeStruct::init() {
// CHECK-NEXT: return _impl::_impl_LargeStruct::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:   _impl::$s4Init11LargeStructVACycfC(result);
// CHECK-NEXT: });
// CHECK-NEXT: }
// CHECK-NEXT: inline LargeStruct LargeStruct::init(swift::Int x, const FirstSmallStruct& y) {
// CHECK-NEXT: return _impl::_impl_LargeStruct::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:   _impl::$s4Init11LargeStructV1x1yACSi_AA010FirstSmallC0VtcfC(result, x, _impl::swift_interop_passDirect_Init_uint32_t_0_4(_impl::_impl_FirstSmallStruct::getOpaquePointer(y)));
// CHECK-NEXT: });
// CHECK-NEXT: }

// CHECK:      inline StructWithRefCountStoredProp StructWithRefCountStoredProp::init() {
// CHECK-NEXT: return _impl::_impl_StructWithRefCountStoredProp::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:   _impl::swift_interop_returnDirect_Init_[[PTRENC]](result, _impl::$s4Init28StructWithRefCountStoredPropVACycfC());
// CHECK-NEXT: });
// CHECK-NEXT: }
// CHECK-NEXT: inline StructWithRefCountStoredProp StructWithRefCountStoredProp::init(swift::Int x) {
// CHECK-NEXT: return _impl::_impl_StructWithRefCountStoredProp::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:   _impl::swift_interop_returnDirect_Init_[[PTRENC]](result, _impl::$s4Init28StructWithRefCountStoredPropV1xACSi_tcfC(x));
// CHECK-NEXT: });
// CHECK-NEXT: }
