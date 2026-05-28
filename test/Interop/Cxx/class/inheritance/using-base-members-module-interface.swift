// RUN: %target-swift-ide-test -print-module -module-to-print=UsingBaseMembers -print-access -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers | %FileCheck %s
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

// CHECK:      public struct PublicBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private var value: CInt
// CHECK-NEXT:   public func publicGetter() -> CInt
// CHECK-NEXT:   public mutating func publicSetter(_ v: CInt)
// CHECK-NEXT:   public func notExposed()
// CHECK-NEXT: }

// CHECK:      public struct PublicBasePrivateInheritance {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @available(*, unavailable, message: "this base member is not accessible because it is private")
// CHECK-NEXT:   private var value: CInt
// CHECK-NEXT:   public func publicGetter() -> CInt
// CHECK-NEXT:   public mutating func publicSetter(_ v: CInt)
// CHECK-NEXT:   private func notExposed()
// CHECK-NEXT: }

// CHECK:      public struct PublicBaseProtectedInheritance {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @available(*, unavailable, message: "this base member is not accessible because it is private")
// CHECK-NEXT:   private var value: CInt
// CHECK-NEXT:   public func publicGetter() -> CInt
// CHECK-NEXT:   public mutating func publicSetter(_ v: CInt)
// CHECK-NEXT:   private func notExposed()
// CHECK-NEXT: }

// CHECK:      public struct PublicBaseUsingPrivateTypedef {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @available(*, unavailable, message: "this base member is not accessible because it is private")
// CHECK-NEXT:   private var value: CInt
// CHECK-NEXT:   public func publicGetter() -> CInt
// CHECK-NEXT:   public mutating func publicSetter(_ v: CInt)
// CHECK-NEXT:   private func notExposed()
// CHECK-NEXT:   private typealias MyBase = PublicBase
// CHECK-NEXT: }

// CHECK:      public struct PublicBaseUsingPrivateUsingType {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @available(*, unavailable, message: "this base member is not accessible because it is private")
// CHECK-NEXT:   private var value: CInt
// CHECK-NEXT:   public func publicGetter() -> CInt
// CHECK-NEXT:   public mutating func publicSetter(_ v: CInt)
// CHECK-NEXT:   private func notExposed()
// CHECK-NEXT:   private typealias MyBase = PublicBase
// CHECK-NEXT: }

// CHECK:      public struct UsingBaseConstructorWithParam {
// CHECK-NEXT:   public init(_: CUnsignedInt)
// CHECK-NEXT:   public init(_: CInt)
// CHECK-NEXT:   public var value: CInt
// CHECK-NEXT: }

// CHECK:      public struct UsingBaseConstructorEmpty {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public var value: CInt
// CHECK-NEXT: }

// CHECK:      public struct ProtectedBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func protectedGetter() -> CInt
// CHECK-NEXT: }

// CHECK:      public struct ProtectedMemberPrivateInheritance {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func protectedGetter() -> CInt
// CHECK-NEXT: }

// CHECK:      public struct OperatorBasePrivateInheritance : CxxConvertibleToBool {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @available(*, deprecated, message: "use Bool(fromCxx:)")
// CHECK-NEXT:   public func __convertToBool() -> CBool
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   public func __operatorStar() -> CInt
// CHECK-NEXT:   @available(*, unavailable, message: "use ! instead")
// CHECK-NEXT:   public func __operatorExclaim() -> OperatorBase
// CHECK-NEXT:   @available(*, unavailable, message: "use subscript")
// CHECK-NEXT:   public func __operatorSubscriptConst(_ x: CInt) -> CInt
// CHECK-NEXT:   prefix public static func ! (lhs: OperatorBasePrivateInheritance) -> OperatorBase
// CHECK-NEXT:   public var pointee: CInt { get }
// CHECK-NEXT:   public subscript(x: CInt) -> CInt { get }
// CHECK-NEXT: }

// CHECK:      public struct OperatorBaseProtectedInheritance {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   private func __operatorStar() -> CInt
// CHECK-NEXT:   private func __operatorExclaim() -> OperatorBase
// CHECK-NEXT:   @available(*, unavailable, message: "use subscript")
// CHECK-NEXT:   private func __operatorSubscriptConst(_ x: CInt) -> CInt
// CHECK-NEXT:   private var pointee: CInt { get }
// CHECK-NEXT:   private subscript(x: CInt) -> CInt { get }
// CHECK-NEXT: }

// CHECK:      public struct ProtectedOperatorBoolBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT: }

// CHECK:      public struct ProtectedOperatorBoolMadePublic : CxxConvertibleToBool {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @available(*, deprecated, message: "use Bool(fromCxx:)")
// CHECK-NEXT:   public func __convertToBool() -> CBool
// CHECK-NEXT: }
