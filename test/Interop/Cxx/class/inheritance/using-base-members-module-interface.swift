// RUN: %target-swift-ide-test -print-module -module-to-print=UsingBaseMembers -print-access -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-6 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=UsingBaseMembers -print-access -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK:      public struct PublicBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publicGetter() -> Int32
// CHECK-NEXT:   public mutating func publicSetter(_ v: Int32)
// CHECK-NEXT:   public func notExposed()
// CHECK-NEXT:   private var value: Int32
// CHECK-NEXT: }

// CHECK:      public struct PublicBasePrivateInheritance {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publicGetter() -> Int32
// CHECK-NEXT:   public mutating func publicSetter(_ v: Int32)
// CHECK-NEXT:   private func notExposed()
// CHECK-NEXT:   @available(*, unavailable, message: "this base member is not accessible because it is private")
// CHECK-NEXT:   private var value: Int32
// CHECK-NEXT: }

// CHECK:      public struct PublicBaseProtectedInheritance {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publicGetter() -> Int32
// CHECK-NEXT:   public mutating func publicSetter(_ v: Int32)
// CHECK-NEXT:   private func notExposed()
// CHECK-NEXT:   @available(*, unavailable, message: "this base member is not accessible because it is private")
// CHECK-NEXT:   private var value: Int32
// CHECK-NEXT: }

// CHECK:      public struct UsingBaseConstructorWithParam {
// CHECK-NEXT:   public init(consuming _: consuming IntBox)
// CHECK-NEXT:   public init(_: IntBox)
// CHECK-NEXT:   public init(_: UInt32)
// CHECK-NEXT:   public init(_: Int32)
// CHECK-NEXT:   public var value: Int32
// CHECK-NEXT: }

// CHECK:      public struct UsingBaseConstructorEmpty {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public init(consuming _: consuming Empty)
// CHECK-NEXT:   public init(_: Empty)
// CHECK-NEXT:   public var value: Int32
// CHECK-NEXT: }

// CHECK:      public struct ProtectedBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func protectedGetter() -> Int32
// CHECK-NEXT: }

// CHECK:      public struct ProtectedMemberPrivateInheritance {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func protectedGetter() -> Int32
// CHECK-NEXT: }
