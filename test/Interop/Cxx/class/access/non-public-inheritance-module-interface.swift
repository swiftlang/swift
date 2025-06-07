// This test checks that the module interface correctly reflects the access
// levels of imported inherited members (especially non-public ones).
//
// For private base members and other base members made inaccessible due to
// private inheritance, this test does not check against a specific message for
// the @available(*, unavailable) attributes (to ensure this test is robust
// against message wording changes), but it does require the message to mention
// something about "private".
//
// RUN: %target-swift-ide-test -print-module -module-to-print=NonPublicInheritance -print-access -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-experimental-feature ImportNonPublicCxxMembers | %FileCheck %s
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

// CHECK:      public struct Base {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PublBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct ProtBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PrivBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PublPublBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct ProtPublBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PrivPublBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PublProtBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct ProtProtBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PrivProtBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PublPrivBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct ProtPrivBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PrivPrivBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT:   @available(*, unavailable, message: "{{.*}}private{{.*}}")
// CHECK-NEXT:   private func priv() -> Int32
// CHECK-NEXT: }
