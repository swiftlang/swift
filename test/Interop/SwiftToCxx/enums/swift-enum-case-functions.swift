// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Enums -clang-header-expose-public-decls -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function)

// test case-related member functions: operator cases() and isXYZ predicates

public enum DataCase { case one(_ x: Int) }

public func makeDataCase() -> DataCase { return .one(10) }

public enum CLikeEnum { case one, two, three }

public func makeCLikeEnum(_ tag: Int) -> CLikeEnum {
    switch tag {
    case 1:
        return .one
    case 2:
        return .two
    default:
        return .three
    }
}

public func checkCLikeEnum(_ x: CLikeEnum, tag: Int) -> Bool {
    switch x {
    case .one:
        return tag == 1
    case .two:
        return tag == 2
    case .three:
        return ![1, 2].contains(tag)
    }
}

public enum BoolWithCase {
    case first
    case second(Bool)
    case third
}

public func makeBoolWithCase(_ tag: Int) -> BoolWithCase {
    switch tag {
    case 1:
        return .first
    case 2:
        return .second(true)
    default:
        return .third
    }
}

public func checkBoolWithCase(_ x: BoolWithCase, tag: Int) -> Bool {
    switch x {
    case .first:
        return tag == 1
    case .second:
        return tag == 2
    case .third:
        return ![1, 2].contains(tag)
    }
}

public enum IntOrInfinity {
    case NegInfinity
    case Int(Int)
    case PosInfinity
}

public func makeIntOrInfinity(_ tag: Int) -> IntOrInfinity {
    switch tag {
    case 1:
        return .NegInfinity
    case 2:
        return .Int(123)
    default:
        return .PosInfinity
    }
}

public func checkIntOrInfinity(_ x: IntOrInfinity, tag: Int) -> Bool {
    switch x {
    case .NegInfinity:
        return tag == 1
    case .Int:
        return tag == 2
    case .PosInfinity:
        return ![1, 2].contains(tag)
    }
}

public enum MultipleBoolWithCase {
    case first
    case second(Bool)
    case third(Bool)
    case fourth
}

public func makeMultipleBoolWithCase(_ tag: Int) -> MultipleBoolWithCase {
    switch tag {
    case 1:
        return .first
    case 2:
        return .second(true)
    case 3:
        return .third(false)
    default:
        return .fourth
    }
}

public func checkMultipleBoolWithCase(_ x: MultipleBoolWithCase, tag: Int) -> Bool {
    switch x {
    case .first:
        return tag == 1
    case .second:
        return tag == 2
    case .third:
        return tag == 3
    case .fourth:
        return ![1, 2, 3].contains(tag)
    }
}

public enum IntDoubleOrBignum {
    case Int(Int)
    case Double(Double)
    case Bignum
}

public func makeIntDoubleOrBignum(_ tag: Int) -> IntDoubleOrBignum {
    switch tag {
    case 1:
        return .Int(10)
    case 2:
        return .Double(3.14)
    default:
        return .Bignum
    }
}

public func checkIntDoubleOrBignum(_ x: IntDoubleOrBignum, tag: Int) -> Bool {
    switch x {
    case .Int:
        return tag == 1
    case .Double:
        return tag == 2
    case .Bignum:
        return ![1, 2].contains(tag)
    }
}

// CHECK: class BoolWithCase final {

// CHECK:      inline operator cases() const {
// CHECK-NEXT:   switch (_getEnumTag()) {
// CHECK-NEXT:     case 0: return cases::second;
// CHECK-NEXT:     case 1: return cases::first;
// CHECK-NEXT:     case 2: return cases::third;
// CHECK-NEXT:     default: abort();
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isSecond() const {
// CHECK-NEXT:   return *this == cases::second;
// CHECK-NEXT: }
// CHECK:      inline bool isFirst() const {
// CHECK-NEXT:   return *this == cases::first;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isThird() const {
// CHECK-NEXT:     return *this == cases::third;
// CHECK-NEXT: }
// CHECK:      inline int _getEnumTag() const {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums12BoolWithCaseOMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   return enumVWTable->getEnumTag(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }

// CHECK: class CLikeEnum final {

// CHECK:      inline operator cases() const {
// CHECK-NEXT:   switch (_getEnumTag()) {
// CHECK-NEXT:     case 0: return cases::one;
// CHECK-NEXT:     case 1: return cases::two;
// CHECK-NEXT:     case 2: return cases::three;
// CHECK-NEXT:     default: abort();
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isOne() const {
// CHECK-NEXT:   return *this == cases::one;
// CHECK-NEXT: }
// CHECK:      inline bool isTwo() const {
// CHECK-NEXT:   return *this == cases::two;
// CHECK-NEXT: }
// CHECK:      inline bool isThree() const {
// CHECK-NEXT:   return *this == cases::three;
// CHECK-NEXT: }
// CHECK:      inline int _getEnumTag() const {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums9CLikeEnumOMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   return enumVWTable->getEnumTag(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }

// CHECK: class DataCase final {

// CHECK:      inline operator cases() const {
// CHECK-NEXT:   switch (_getEnumTag()) {
// CHECK-NEXT:     case 0: return cases::one;
// CHECK-NEXT:     default: abort();
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isOne() const {
// CHECK-NEXT:   return *this == cases::one;
// CHECK-NEXT: }
// CHECK:      inline int _getEnumTag() const {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums8DataCaseOMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   return enumVWTable->getEnumTag(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }

// CHECK: class IntDoubleOrBignum final {

// CHECK:      inline operator cases() const {
// CHECK-NEXT:   switch (_getEnumTag()) {
// CHECK-NEXT:     case 0: return cases::Int;
// CHECK-NEXT:     case 1: return cases::Double;
// CHECK-NEXT:     case 2: return cases::Bignum;
// CHECK-NEXT:     default: abort();
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isInt() const {
// CHECK-NEXT:   return *this == cases::Int;
// CHECK-NEXT: }
// CHECK:      inline bool isDouble() const {
// CHECK-NEXT:   return *this == cases::Double;
// CHECK-NEXT: }
// CHECK:      inline bool isBignum() const {
// CHECK-NEXT:   return *this == cases::Bignum;
// CHECK-NEXT: }
// CHECK:      inline int _getEnumTag() const {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums17IntDoubleOrBignumOMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   return enumVWTable->getEnumTag(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }

// CHECK: class IntOrInfinity final {

// CHECK:      inline operator cases() const {
// CHECK-NEXT:   switch (_getEnumTag()) {
// CHECK-NEXT:     case 0: return cases::Int;
// CHECK-NEXT:     case 1: return cases::NegInfinity;
// CHECK-NEXT:     case 2: return cases::PosInfinity;
// CHECK-NEXT:     default: abort();
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isInt() const {
// CHECK-NEXT:   return *this == cases::Int;
// CHECK-NEXT: }
// CHECK:      inline bool isNegInfinity() const {
// CHECK-NEXT:   return *this == cases::NegInfinity;
// CHECK-NEXT: }
// CHECK:      inline bool isPosInfinity() const {
// CHECK-NEXT:   return *this == cases::PosInfinity;
// CHECK-NEXT: }
// CHECK:      inline int _getEnumTag() const {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums13IntOrInfinityOMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   return enumVWTable->getEnumTag(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }

// CHECK: class MultipleBoolWithCase final {

// CHECK:      inline operator cases() const {
// CHECK-NEXT:     switch (_getEnumTag()) {
// CHECK-NEXT:     case 0: return cases::second;
// CHECK-NEXT:     case 1: return cases::third;
// CHECK-NEXT:     case 2: return cases::first;
// CHECK-NEXT:     case 3: return cases::fourth;
// CHECK-NEXT:     default: abort();
// CHECK-NEXT:     }
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isSecond() const {
// CHECK-NEXT:     return *this == cases::second;
// CHECK-NEXT: }
// CHECK:      inline bool isThird() const {
// CHECK-NEXT:     return *this == cases::third;
// CHECK-NEXT: }
// CHECK:      inline bool isFirst() const {
// CHECK-NEXT:     return *this == cases::first;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isFourth() const {
// CHECK-NEXT:     return *this == cases::fourth;
// CHECK-NEXT: }
// CHECK:      inline int _getEnumTag() const {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums20MultipleBoolWithCaseOMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   return enumVWTable->getEnumTag(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }
