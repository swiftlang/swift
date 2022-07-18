// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Properties -clang-header-expose-public-decls -emit-clang-header-path %t/properties.h
// RUN: %FileCheck %s < %t/properties.h

// RUN: %check-interop-cxx-header-in-clang(%t/properties.h)

public struct FirstSmallStruct {
    public var x: UInt32
}

// CHECK: class FirstSmallStruct final {
// CHECK: public:
// CHECK:   inline FirstSmallStruct(FirstSmallStruct &&) = default;
// CHECK-NEXT:   inline uint32_t getX() const;
// CHECK-NEXT:   inline void setX(uint32_t value);
// CHECK-NEXT:   private:

// CHECK: inline uint32_t FirstSmallStruct::getX() const {
// CHECK-NEXT: return _impl::$s10Properties16FirstSmallStructV1xs6UInt32Vvg(_impl::swift_interop_passDirect_Properties_FirstSmallStruct(_getOpaquePointer()));
// CHECK-NEXT: }
// CHECK-NEXT: inline void FirstSmallStruct::setX(uint32_t value) {
// CHECK-NEXT: return _impl::$s10Properties16FirstSmallStructV1xs6UInt32Vvs(value, _getOpaquePointer());
// CHECK-NEXT: }

public struct LargeStruct {
    public var x1, x2, x3, x4, x5, x6: Int
}

// CHECK: class LargeStruct final {
// CHECK: public:
// CHECK: inline LargeStruct(LargeStruct &&) = default;
// CHECK-NEXT: inline swift::Int getX1() const;
// CHECK-NEXT: inline void setX1(swift::Int value);
// CHECK-NEXT: inline swift::Int getX2() const;
// CHECK-NEXT: inline void setX2(swift::Int value);
// CHECK-NEXT: inline swift::Int getX3() const;
// CHECK-NEXT: inline void setX3(swift::Int value);
// CHECK-NEXT: inline swift::Int getX4() const;
// CHECK-NEXT: inline void setX4(swift::Int value);
// CHECK-NEXT: inline swift::Int getX5() const;
// CHECK-NEXT: inline void setX5(swift::Int value);
// CHECK-NEXT: inline swift::Int getX6() const;
// CHECK-NEXT: inline void setX6(swift::Int value);
// CHECK-NEXT: private:

// CHECK:        inline swift::Int LargeStruct::getX1() const {
// CHECK-NEXT:   return _impl::$s10Properties11LargeStructV2x1Sivg(_getOpaquePointer());
// CHECK-NEXT:   }
// CHECK-NEXT:   inline void LargeStruct::setX1(swift::Int value) {
// CHECK-NEXT:   return _impl::$s10Properties11LargeStructV2x1Sivs(value, _getOpaquePointer());
// CHECK-NEXT:   }

public struct LargeStructWithProps {
    public var storedLargeStruct: LargeStruct
    public var storedSmallStruct: FirstSmallStruct
}

// CHECK:      class LargeStructWithProps final {
// CHECK-NEXT: public:
// CHECK:      inline LargeStruct getStoredLargeStruct() const;
// CHECK-NEXT: inline void setStoredLargeStruct(const LargeStruct& value);
// CHECK-NEXT: inline FirstSmallStruct getStoredSmallStruct() const;
// CHECK-NEXT: inline void setStoredSmallStruct(const FirstSmallStruct& value);

// CHECK:        inline LargeStruct LargeStructWithProps::getStoredLargeStruct() const {
// CHECK-NEXT:    return _impl::_impl_LargeStruct::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:     _impl::$s10Properties20LargeStructWithPropsV06storedbC0AA0bC0Vvg(result, _getOpaquePointer());
// CHECK-NEXT:   });
// CHECK-NEXT:   }
// CHECK-NEXT:   inline void LargeStructWithProps::setStoredLargeStruct(const LargeStruct& value) {
// CHECK-NEXT:   return _impl::$s10Properties20LargeStructWithPropsV06storedbC0AA0bC0Vvs(_impl::_impl_LargeStruct::getOpaquePointer(value), _getOpaquePointer());
// CHECK-NEXT:   }
// CHECK-NEXT:   inline FirstSmallStruct LargeStructWithProps::getStoredSmallStruct() const {
// CHECK-NEXT:   return _impl::_impl_FirstSmallStruct::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:     _impl::swift_interop_returnDirect_Properties_FirstSmallStruct(result, _impl::$s10Properties20LargeStructWithPropsV011storedSmallC0AA05FirstgC0Vvg(_getOpaquePointer()));
// CHECK-NEXT:   });
// CHECK-NEXT:   }
// CHECK-NEXT:   inline void LargeStructWithProps::setStoredSmallStruct(const FirstSmallStruct& value) {
// CHECK-NEXT:   return _impl::$s10Properties20LargeStructWithPropsV011storedSmallC0AA05FirstgC0Vvs(_impl::swift_interop_passDirect_Properties_FirstSmallStruct(_impl::_impl_FirstSmallStruct::getOpaquePointer(value)), _getOpaquePointer());
// CHECK-NEXT:   }

public struct SmallStructWithProps {
    public var storedInt: UInt32
    public var computedInt: Int {
        get {
            return Int(storedInt) + 2
        } set {
            storedInt = UInt32(newValue - 2)
        }
    }

    public var largeStructWithProps: LargeStructWithProps {
        get {
            return LargeStructWithProps(storedLargeStruct: LargeStruct(x1: computedInt * 2, x2: 1, x3: 2, x4: 3, x5: 4, x6: 5),
                                        storedSmallStruct:FirstSmallStruct(x: 0xFAE))
        } set {
            print("SET: \(newValue.storedLargeStruct), \(newValue.storedSmallStruct)")
        }
    }
}

// CHECK: class SmallStructWithProps final {
// CHECK: public:
// CHECK:   inline SmallStructWithProps(SmallStructWithProps &&) = default;
// CHECK-NEXT:    inline uint32_t getStoredInt() const;
// CHECK-NEXT:    inline void setStoredInt(uint32_t value);
// CHECK-NEXT:    inline swift::Int getComputedInt() const;
// CHECK-NEXT:    inline void setComputedInt(swift::Int newValue);
// CHECK-NEXT:    inline LargeStructWithProps getLargeStructWithProps() const;
// CHECK-NEXT:    inline void setLargeStructWithProps(const LargeStructWithProps& newValue);
// CHECK-NEXT: private:

// CHECK:        inline uint32_t SmallStructWithProps::getStoredInt() const {
// CHECK-NEXT:  return _impl::$s10Properties20SmallStructWithPropsV9storedInts6UInt32Vvg(_impl::swift_interop_passDirect_Properties_SmallStructWithProps(_getOpaquePointer()));
// CHECK-NEXT:  }
// CHECK-NEXT:  inline void SmallStructWithProps::setStoredInt(uint32_t value) {
// CHECK-NEXT:  return _impl::$s10Properties20SmallStructWithPropsV9storedInts6UInt32Vvs(value, _getOpaquePointer());
// CHECK-NEXT:  }
// CHECK-NEXT:  inline swift::Int SmallStructWithProps::getComputedInt() const {
// CHECK-NEXT:  return _impl::$s10Properties20SmallStructWithPropsV11computedIntSivg(_impl::swift_interop_passDirect_Properties_SmallStructWithProps(_getOpaquePointer()));
// CHECK-NEXT:  }
// CHECK-NEXT:  inline void SmallStructWithProps::setComputedInt(swift::Int newValue) {
// CHECK-NEXT:  return _impl::$s10Properties20SmallStructWithPropsV11computedIntSivs(newValue, _getOpaquePointer());
// CHECK-NEXT:  }
// CHECK-NEXT:  inline LargeStructWithProps SmallStructWithProps::getLargeStructWithProps() const {
// CHECK-NEXT:  return _impl::_impl_LargeStructWithProps::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:    _impl::$s10Properties20SmallStructWithPropsV05largecdE0AA05LargecdE0Vvg(result, _impl::swift_interop_passDirect_Properties_SmallStructWithProps(_getOpaquePointer()));
// CHECK-NEXT:  });
// CHECK-NEXT:  }
// CHECK-NEXT:  inline void SmallStructWithProps::setLargeStructWithProps(const LargeStructWithProps& newValue) {
// CHECK-NEXT:  return _impl::$s10Properties20SmallStructWithPropsV05largecdE0AA05LargecdE0Vvs(_impl::_impl_LargeStructWithProps::getOpaquePointer(newValue), _getOpaquePointer());
// CHECK-NEXT:  }

public func createSmallStructWithProps() -> SmallStructWithProps {
    return SmallStructWithProps(storedInt: 21)
}

public func createFirstSmallStruct(_ x: UInt32) -> FirstSmallStruct {
    return FirstSmallStruct(x: x)
}
