// Test module interface produced for C++ access specifiers test.
// In particular, we don't want any of the private members showing up here.

// RUN: %target-swift-ide-test -print-module -module-to-print=AccessSpecifiers -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK:      struct PublicPrivate {
// CHECK-NEXT:   typealias PublicTypedef = Int32
// CHECK-NEXT:   struct PublicStruct {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT:   struct PublicEnum : Equatable, RawRepresentable {
// CHECK-NEXT:     init(_ rawValue: [[ENUM_UNDERLYING_TYPE:Int32|UInt32]])
// CHECK-NEXT:     init(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     var rawValue: [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:   }
// CHECK-NEXT:   @frozen enum PublicClosedEnum : [[ENUM_UNDERLYING_TYPE]] {
// CHECK-NEXT:     init?(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     var rawValue: [[ENUM_UNDERLYING_TYPE]] { get }
// CHECK-NEXT:     typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     case value1
// CHECK-NEXT:     @available(swift, obsoleted: 3, renamed: "value1")
// CHECK-NEXT:     static var Value1: PublicPrivate.PublicClosedEnum { get }
// CHECK-NEXT:   }
// CHECK-NEXT:   enum PublicOpenEnum : [[ENUM_UNDERLYING_TYPE]] {
// CHECK-NEXT:     init?(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     var rawValue: [[ENUM_UNDERLYING_TYPE]] { get }
// CHECK-NEXT:     typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     case value1
// CHECK-NEXT:     @available(swift, obsoleted: 3, renamed: "value1")
// CHECK-NEXT:     static var Value1: PublicPrivate.PublicOpenEnum { get }
// CHECK-NEXT:   }
// CHECK-NEXT:   struct PublicFlagEnum : OptionSet {
// CHECK-NEXT:     init(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     let rawValue: [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     typealias Element = PublicPrivate.PublicFlagEnum
// CHECK-NEXT:     typealias ArrayLiteralElement = PublicPrivate.PublicFlagEnum
// CHECK-NEXT:   }
// CHECK-NEXT:   static var PublicStaticMemberVar: Int32
// CHECK-NEXT:   var PublicMemberVar: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT:   mutating func publicMemberFunc()
// CHECK-NEXT: }
