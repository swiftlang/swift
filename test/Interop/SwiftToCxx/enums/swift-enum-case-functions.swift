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

public enum CharOrSectionMarker {
    case Paragraph
    case Char(Unicode.Scalar)
    case Chapter
}

public func makeCharOrSectionMarker(_ tag: Int) -> CharOrSectionMarker {
    switch tag {
    case 1:
        return .Paragraph
    case 2:
        return .Char("🍎")
    default:
        return .Chapter
    }
}

public func checkCharOrSectionMarker(_ x: CharOrSectionMarker, tag: Int) -> Bool {
    switch x {
    case .Paragraph:
        return tag == 1
    case .Char:
        return tag == 2
    case .Chapter:
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

public enum TerminalChar {
    case Cursor
    case Plain(Unicode.Scalar)
    case Bold(Unicode.Scalar)
    case Underline(Unicode.Scalar)
    case Blink(Unicode.Scalar)
    case Empty
}

public func makeTerminalChar(_ tag: Int) -> TerminalChar {
    switch tag {
    case 1:
        return .Plain("P")
    case 2:
        return .Bold("B")
    case 3:
        return .Underline("U")
    case 4:
        return .Blink("L")
    case 5:
        return .Empty
    default:
        return .Cursor
    }
}

public func checkTerminalChar(_ x: TerminalChar, tag: Int) -> Bool {
    switch x {
    case .Plain:
        return tag == 1
    case .Bold:
        return tag == 2
    case .Underline:
        return tag == 3
    case .Blink:
        return tag == 4
    case .Empty:
        return tag == 5
    case .Cursor:
        return ![1, 2, 3, 4, 5].contains(tag)
    }
}

public class Bignum {}

public enum IntDoubleOrBignum {
    case Int(Int)
    case Double(Double)
    case Bignum(Bignum)
}

public func makeIntDoubleOrBignum(_ tag: Int) -> IntDoubleOrBignum {
    switch tag {
    case 1:
        return .Int(10)
    case 2:
        return .Double(3.14)
    default:
        return .Bignum(Bignum())
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

// CHECK: class CLikeEnum final {

// CHECK:      inline operator cases() const {
// CHECK-NEXT:   switch (_getEnumTag()) {
// CHECK-NEXT:     case 0: return cases::one;
// CHECK-NEXT:     case 1: return cases::two;
// CHECK-NEXT:     case 2: default: return cases::three;
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isOne() const {
// CHECK-NEXT:   return *this == cases::one;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isTwo() const {
// CHECK-NEXT:   return *this == cases::two;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isThree() const {
// CHECK-NEXT:   return *this == cases::three;
// CHECK-NEXT: }
// CHECK:      inline int _getEnumTag() const {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums9CLikeEnumOMa(0);
// CHECK-NEXT:   auto *vwTable = *(reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1);
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   return enumVWTable->getEnumTag(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }

// CHECK: class CharOrSectionMarker final {

// CHECK:      inline operator cases() const {
// CHECK-NEXT:   switch (_getEnumTag()) {
// CHECK-NEXT:     case 1: return cases::Paragraph;
// CHECK-NEXT:     case 0: return cases::Char;
// CHECK-NEXT:     case 2: default: return cases::Chapter;
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isParagraph() const {
// CHECK-NEXT:   return *this == cases::Paragraph;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isChar() const {
// CHECK-NEXT:   return *this == cases::Char;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isChapter() const {
// CHECK-NEXT:   return *this == cases::Chapter;
// CHECK-NEXT: }
// CHECK:      inline int _getEnumTag() const {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums19CharOrSectionMarkerOMa(0);
// CHECK-NEXT:   auto *vwTable = *(reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1);
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   return enumVWTable->getEnumTag(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }

// CHECK: class DataCase final {

// CHECK:      inline operator cases() const {
// CHECK-NEXT:   switch (_getEnumTag()) {
// CHECK-NEXT:     case 0: default: return cases::one;
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isOne() const {
// CHECK-NEXT:   return *this == cases::one;
// CHECK-NEXT: }
// CHECK:      inline int _getEnumTag() const {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums8DataCaseOMa(0);
// CHECK-NEXT:   auto *vwTable = *(reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1);
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   return enumVWTable->getEnumTag(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }

// CHECK: class IntDoubleOrBignum final {

// CHECK:      inline operator cases() const {
// CHECK-NEXT:   switch (_getEnumTag()) {
// CHECK-NEXT:     case 0: return cases::Int;
// CHECK-NEXT:     case 1: return cases::Double;
// CHECK-NEXT:     case 2: default: return cases::Bignum;
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isInt() const {
// CHECK-NEXT:   return *this == cases::Int;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isDouble() const {
// CHECK-NEXT:   return *this == cases::Double;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isBignum() const {
// CHECK-NEXT:   return *this == cases::Bignum;
// CHECK-NEXT: }
// CHECK:      inline int _getEnumTag() const {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums17IntDoubleOrBignumOMa(0);
// CHECK-NEXT:   auto *vwTable = *(reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1);
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   return enumVWTable->getEnumTag(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }

// CHECK: class IntOrInfinity final {

// CHECK:      inline operator cases() const {
// CHECK-NEXT:   switch (_getEnumTag()) {
// CHECK-NEXT:     case 1: return cases::NegInfinity;
// CHECK-NEXT:     case 0: return cases::Int;
// CHECK-NEXT:     case 2: default: return cases::PosInfinity;
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isNegInfinity() const {
// CHECK-NEXT:   return *this == cases::NegInfinity;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isInt() const {
// CHECK-NEXT:   return *this == cases::Int;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isPosInfinity() const {
// CHECK-NEXT:   return *this == cases::PosInfinity;
// CHECK-NEXT: }
// CHECK:      inline int _getEnumTag() const {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums13IntOrInfinityOMa(0);
// CHECK-NEXT:   auto *vwTable = *(reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1);
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   return enumVWTable->getEnumTag(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }

// CHECK: class TerminalChar final {

// CHECK:      inline operator cases() const {
// CHECK-NEXT:   switch (_getEnumTag()) {
// CHECK-NEXT:     case 4: return cases::Cursor;
// CHECK-NEXT:     case 0: return cases::Plain;
// CHECK-NEXT:     case 1: return cases::Bold;
// CHECK-NEXT:     case 2: return cases::Underline;
// CHECK-NEXT:     case 3: return cases::Blink;
// CHECK-NEXT:     case 5: default: return cases::Empty;
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isCursor() const {
// CHECK-NEXT:   return *this == cases::Cursor;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isPlain() const {
// CHECK-NEXT:   return *this == cases::Plain;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isBold() const {
// CHECK-NEXT:   return *this == cases::Bold;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isUnderline() const {
// CHECK-NEXT:   return *this == cases::Underline;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isBlink() const {
// CHECK-NEXT:   return *this == cases::Blink;
// CHECK-NEXT: }
// CHECK-NEXT: inline bool isEmpty() const {
// CHECK-NEXT:   return *this == cases::Empty;
// CHECK-NEXT: }
// CHECK:      inline int _getEnumTag() const {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums12TerminalCharOMa(0);
// CHECK-NEXT:   auto *vwTable = *(reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1);
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   return enumVWTable->getEnumTag(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }
