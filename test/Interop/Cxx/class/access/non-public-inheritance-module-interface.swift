// RUN: %target-swift-ide-test -print-module -module-to-print=NonPublicInheritance -print-access -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

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
// CHECK-NEXT: }

// CHECK-NEXT: public struct ProtBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PrivBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PublPublBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct ProtPublBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PrivPublBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PublProtBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct ProtProtBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PrivProtBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private func publ() -> Int32
// CHECK-NEXT:   private func prot() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct PublPrivBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT: }

// CHECK-NEXT: public struct ProtPrivBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT: }

// CHECK-NEXT: public struct PrivPrivBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT: }

