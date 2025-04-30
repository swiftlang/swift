// RUN: %target-swift-ide-test -print-module -module-to-print=UsingNonPublic -print-access -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers | %FileCheck %s
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

// CHECK:      public struct PublUser {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publUsingProt() -> Return
// CHECK-NEXT:   public func publUsingPubl() -> Return
// CHECK-NEXT:   private func protUsingProt() -> Return
// CHECK-NEXT:   private func protUsingPubl() -> Return
// CHECK-NEXT:   private func omitUsingProt() -> Return
// CHECK-NEXT:   public func omitUsingPubl() -> Return
// CHECK-NEXT: }

// CHECK:      public struct ProtUser {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publUsingProt() -> Return
// CHECK-NEXT:   public func publUsingPubl() -> Return
// CHECK-NEXT:   private func protUsingProt() -> Return
// CHECK-NEXT:   private func protUsingPubl() -> Return
// CHECK-NEXT:   private func omitUsingProt() -> Return
// CHECK-NEXT:   private func omitUsingPubl() -> Return
// CHECK-NEXT: }

// CHECK:      public struct PrivUser {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publUsingProt() -> Return
// CHECK-NEXT:   public func publUsingPubl() -> Return
// CHECK-NEXT:   private func protUsingProt() -> Return
// CHECK-NEXT:   private func protUsingPubl() -> Return
// CHECK-NEXT:   private func omitUsingProt() -> Return
// CHECK-NEXT:   private func omitUsingPubl() -> Return
// CHECK-NEXT: }

// CHECK:      public struct PublPrivUser {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publUsingProt() -> Return
// CHECK-NEXT:   public func publUsingPubl() -> Return
// CHECK-NEXT:   private func protUsingProt() -> Return
// CHECK-NEXT:   private func protUsingPubl() -> Return
// CHECK-NEXT:   @available(*, unavailable, message: "this base member is not accessible because of private inheritance")
// CHECK-NEXT:   private func omitUsingProt() -> Return
// CHECK-NEXT:   @available(*, unavailable, message: "this base member is not accessible because of private inheritance")
// CHECK-NEXT:   private func omitUsingPubl() -> Return
// CHECK-NEXT: }

// CHECK:      public struct PrivUserPubl {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publUsingProt() -> Return
// CHECK-NEXT:   public func publUsingPubl() -> Return
// CHECK-NEXT:   private func protUsingProt() -> Return
// CHECK-NEXT:   private func protUsingPubl() -> Return
// CHECK-NEXT:   private func omitUsingProt() -> Return
// CHECK-NEXT:   private func omitUsingPubl() -> Return
// CHECK-NEXT: }
