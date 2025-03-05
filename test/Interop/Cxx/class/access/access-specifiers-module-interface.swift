// Test module interface produced for C++ access specifiers test.
// Public C++ members should be imported with Swift-public access, and
// private C++ members should be imported with Swift-private access.

// RUN: %target-swift-ide-test -print-module -module-to-print=AccessSpecifiers -print-access -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-experimental-feature ImportNonPublicCxxMembers | %FileCheck %s
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

// CHECK:      public struct PublicPrivate {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public static var PublicStaticMemberVar: Int32
// CHECK-NEXT:   public mutating func publicMemberFunc()
// CHECK-NEXT:   public typealias PublicTypedef = Int32
// CHECK-NEXT:   public struct PublicStruct {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT:   public struct PublicEnum : Hashable, Equatable, RawRepresentable {
// CHECK-NEXT:     public init(_ rawValue: [[ENUM_UNDERLYING_TYPE:Int32|UInt32]])
// CHECK-NEXT:     public init(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     public var rawValue: [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     public typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:   }
// CHECK-NEXT:   @frozen public enum PublicClosedEnum : [[ENUM_UNDERLYING_TYPE]], @unchecked Sendable {
// CHECK-NEXT:     public init?(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     public var rawValue: [[ENUM_UNDERLYING_TYPE]] { get }
// CHECK-NEXT:     public typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     case value1
// CHECK-NEXT:     @available(swift, obsoleted: 3, renamed: "value1")
// CHECK-NEXT:     public static var Value1: PublicPrivate.PublicClosedEnum { get }
// CHECK-NEXT:   }
// CHECK-NEXT:   public enum PublicOpenEnum : [[ENUM_UNDERLYING_TYPE]], @unchecked Sendable {
// CHECK-NEXT:     public init?(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     public var rawValue: [[ENUM_UNDERLYING_TYPE]] { get }
// CHECK-NEXT:     public typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     case value1
// CHECK-NEXT:     @available(swift, obsoleted: 3, renamed: "value1")
// CHECK-NEXT:     public static var Value1: PublicPrivate.PublicOpenEnum { get }
// CHECK-NEXT:   }
// CHECK-NEXT:   public struct PublicFlagEnum : OptionSet, @unchecked Sendable {
// CHECK-NEXT:     public init(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     public let rawValue: [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     public typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     public typealias Element = PublicPrivate.PublicFlagEnum
// CHECK-NEXT:     public typealias ArrayLiteralElement = PublicPrivate.PublicFlagEnum
// CHECK-NEXT:   }
// CHECK-NEXT:   private static var PrivateStaticMemberVar: Int32
// CHECK-NEXT:   private mutating func privateMemberFunc()
// CHECK-NEXT:   private typealias PrivateTypedef = Int32
// CHECK-NEXT:   private struct PrivateStruct {
// CHECK-NEXT:     public init()
// CHECK-NEXT:   }
// CHECK-NEXT:   private struct PrivateEnum : Hashable, Equatable, RawRepresentable {
// CHECK-NEXT:     private init(_ rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     private init(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     private var rawValue: [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     private typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:   }
// CHECK-NEXT:   @frozen private enum PrivateClosedEnum : [[ENUM_UNDERLYING_TYPE]], @unchecked Sendable {
// CHECK-NEXT:     private init?(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     private var rawValue: [[ENUM_UNDERLYING_TYPE]] { get }
// CHECK-NEXT:     private typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     case value1
// CHECK-NEXT:     @available(swift, obsoleted: 3, renamed: "value1")
// CHECK-NEXT:     private static var Value1: PublicPrivate.PrivateClosedEnum { get }
// CHECK-NEXT:   }
// CHECK-NEXT:   private enum PrivateOpenEnum : [[ENUM_UNDERLYING_TYPE]], @unchecked Sendable {
// CHECK-NEXT:     private init?(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     private var rawValue: [[ENUM_UNDERLYING_TYPE]] { get }
// CHECK-NEXT:     private typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     case value1
// CHECK-NEXT:     @available(swift, obsoleted: 3, renamed: "value1")
// CHECK-NEXT:     private static var Value1: PublicPrivate.PrivateOpenEnum { get }
// CHECK-NEXT:   }
// CHECK-NEXT:   private struct PrivateFlagEnum : OptionSet, @unchecked Sendable {
// CHECK-NEXT:     private init(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     private let rawValue: [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     private typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     private typealias Element = PublicPrivate.PrivateFlagEnum
// CHECK-NEXT:     private typealias ArrayLiteralElement = PublicPrivate.PrivateFlagEnum
// CHECK-NEXT:   }
// CHECK-NEXT:   public var PublicMemberVar: Int32
// CHECK-NEXT:   private var PrivateMemberVar: Int32
// CHECK-NEXT: }
