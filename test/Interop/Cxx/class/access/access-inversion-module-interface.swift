// RUN: %target-swift-ide-test -print-module -print-access -module-to-print=AccessInversion -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers | %FileCheck %s
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

// CHECK: public struct Leaky {

// CHECK:   private typealias PrivateAlias = Bool

// CHECK:   private struct PrivateRec {
// CHECK:     public init()
// CHECK:     private init()
// CHECK:     public func privateRecMethod()
// CHECK:     public static var PRIVATE_REC_CONST: Bool { get }
// CHECK:   }

// CHECK:   private struct PrivateEnum : Hashable, Equatable, RawRepresentable {
// CHECK:     private init(_ rawValue: [[ENUM_RV_T:.*]])
// CHECK:     private init(rawValue: [[ENUM_RV_T]])
// CHECK:     private var rawValue: [[ENUM_RV_T]]
// CHECK:     private typealias RawValue = [[ENUM_RV_T]]
// CHECK:   }

// CHECK:   private enum PrivateEnumClass : [[ENUM_CLASS_RV_T:.*]] {
// CHECK:     private init?(rawValue: [[ENUM_CLASS_RV_T]])
// CHECK:     private var rawValue: [[ENUM_CLASS_RV_T]] { get }
// CHECK:     private typealias RawValue = [[ENUM_CLASS_RV_T]]
// CHECK:     case privateEnumClassMember
// CHECK:   }

// CHECK:   private static var PRIVATE_CONST: Bool { get }

// CHECK:   private static var privateAliasVal: Leaky.PrivateAlias
// CHECK:   private static var privateRecVal: Leaky.PrivateRec
// CHECK:   private static var privateEnumVal: Leaky.PrivateEnum
// CHECK:   private static var privateEnumClassVal: Leaky.PrivateEnumClass

// CHECK:   public typealias AliasToPrivateAlias = Leaky.PrivateAlias
// CHECK:   public typealias AliasToPrivateRec = Leaky.PrivateRec
// CHECK:   public typealias AliasToPrivateEnum = Leaky.PrivateEnum
// CHECK:   public typealias AliasToPrivateEnumClass = Leaky.PrivateEnumClass

// CHECK:   public struct RecWithPrivateAlias {
// CHECK:     public init()
// CHECK:     public init(mem: Leaky.PrivateAlias)
// CHECK:     public var mem: Leaky.PrivateAlias
// CHECK:   }

// CHECK:   public struct RecWithPrivateRec {
// CHECK:     public init()
// CHECK:     public init(mem: Leaky.PrivateRec)
// CHECK:     public var mem: Leaky.PrivateRec
// CHECK:   }

// CHECK:   public struct RecWithPrivateEnum {
// CHECK:     public init()
// CHECK:     public init(mem: Leaky.PrivateEnum)
// CHECK:     public var mem: Leaky.PrivateEnum
// CHECK:   }

// CHECK:   public struct RecWithPrivateEnumClass {
// CHECK:     public init()
// CHECK:     public init(mem: Leaky.PrivateEnumClass)
// CHECK:     public var mem: Leaky.PrivateEnumClass
// CHECK:   }

// CHECK:   public struct RecWithPrivateConst {
// CHECK:     public init()
// CHECK:     public init(mem: Bool)
// CHECK:     public var mem: Bool { get }
// CHECK:   }

// CHECK:   public static func staticReturningPrivateAlias() -> Leaky.PrivateAlias
// CHECK:   public static func staticReturningPrivateRec() -> Leaky.PrivateRec
// CHECK:   public static func staticReturningPrivateEnum() -> Leaky.PrivateEnum
// CHECK:   public static func staticReturningPrivateEnumClass() -> Leaky.PrivateEnumClass

// CHECK:   public static func staticTakingPrivateAlias(_ p: Leaky.PrivateAlias)
// CHECK:   public static func staticTakingPrivateRec(_ p: Leaky.PrivateRec)
// CHECK:   public static func staticTakingPrivateEnum(_ p: Leaky.PrivateEnum)
// CHECK:   public static func staticTakingPrivateEnumClass(_ p: Leaky.PrivateEnumClass)

// CHECK:   public func methodReturningPrivateAlias() -> Leaky.PrivateAlias
// CHECK:   public func methodReturningPrivateRec() -> Leaky.PrivateRec
// CHECK:   public func methodReturningPrivateEnum() -> Leaky.PrivateEnum
// CHECK:   public func methodReturningPrivateEnumClass() -> Leaky.PrivateEnumClass

// CHECK:   public func methodTakingPrivateAlias(_ p: Leaky.PrivateAlias)
// CHECK:   public func methodTakingPrivateRec(_ p: Leaky.PrivateRec)
// CHECK:   public func methodTakingPrivateEnum(_ p: Leaky.PrivateEnum)
// CHECK:   public func methodTakingPrivateEnumClass(_ p: Leaky.PrivateEnumClass)

// CHECK:   public func defaultArgOfPrivateRec(_ a: Leaky.PrivateRec = cxxDefaultArg)
// CHECK:   public func defaultArgOfPrivateEnum(_ a: Leaky.PrivateEnum = cxxDefaultArg)
// CHECK:   public func defaultArgOfPrivateEnumClass(_ a: Leaky.PrivateEnumClass = cxxDefaultArg)
// CHECK:   public func defaultArgOfPrivateConst(_ a: Bool = cxxDefaultArg)
// CHECK:   public func defaultArgOfPrivateRecConst(_ a: Bool = cxxDefaultArg)

// CHECK: }
