// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/TransferableLib.swiftinterface) %s -enable-experimental-feature NontransferableTypes -module-name TransferableLib
// RUN: %target-swift-typecheck-module-from-interface(%t/TransferableLib.swiftinterface) -module-name TransferableLib
// RUN: %FileCheck %s < %t/TransferableLib.swiftinterface

// REQUIRES: swift_feature_NontransferableTypes

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public struct NT : ~Swift::Transferable {
// CHECK-NEXT: }
// CHECK-NEXT: #endif
public struct NT: ~Transferable {}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public class NTClass : ~Swift::Transferable {
// CHECK:      #endif
public class NTClass: ~Transferable {}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public func takesNT<T>(_ t: T) where T : ~Transferable
// CHECK:      #endif
public func takesNT<T: ~Transferable>(_ t: T) {}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public func takesAnyNT(_ t: any ~Transferable)
// CHECK:      #endif
public func takesAnyNT(_ t: any ~Transferable) {}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public protocol HasNT {
// CHECK-NEXT:   associatedtype A : ~Transferable
// CHECK-NEXT: }
// CHECK-NEXT: #endif
public protocol HasNT {
  associatedtype A: ~Transferable
}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public struct Box<T> : ~Swift::Transferable where T : ~Transferable {
// CHECK:      #endif
public struct Box<T: ~Transferable>: ~Transferable {}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: extension TransferableLib::Box : Swift::Transferable where T : Swift::Transferable {
// CHECK:      #endif
extension Box: Transferable where T: Transferable {}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public func takesTransferableA<T>(_ t: T) where T : TransferableLib::HasNT, T.A : Swift::Transferable
// CHECK:      #endif
public func takesTransferableA<T: HasNT>(_ t: T) where T.A: Transferable {}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public struct ExplicitlyTransferable : Swift::Transferable {
// CHECK:      #endif
public struct ExplicitlyTransferable: Transferable {}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public func takesNTClass(_ c: TransferableLib::NTClass)
// CHECK:      #endif
public func takesNTClass(_ c: NTClass) {}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public var ntClassVar: TransferableLib::NTClass {
// CHECK:      #endif
public var ntClassVar: NTClass { NTClass() }

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public typealias NTAlias = TransferableLib::NT
// CHECK:      #endif
public typealias NTAlias = NT

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public class SubOfNTClass : TransferableLib::NTClass {
// CHECK:      #endif
public class SubOfNTClass: NTClass {}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public func takesBoxOfInt(_ b: TransferableLib::Box<Swift::Int>)
// CHECK:      #endif
public func takesBoxOfInt(_ b: Box<Int>) {}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public func takesExplicitlyTransferable(_ e: TransferableLib::ExplicitlyTransferable)
// CHECK:      #endif
public func takesExplicitlyTransferable(_ e: ExplicitlyTransferable) {}

// Through a protocol, and then through a type constrained to it.
// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public struct UsesHasNT<T> where T : TransferableLib::HasNT {
// CHECK:      #endif
public struct UsesHasNT<T: HasNT> {}

// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public func takesUsesHasNT<T>(_ u: TransferableLib::UsesHasNT<T>) where T : TransferableLib::HasNT
// CHECK:      #endif
public func takesUsesHasNT<T>(_ u: UsesHasNT<T>) {}

// The opaque type declaration isn't printed, so it doesn't get a guard of its
// own.
// CHECK:      #if compiler(>=5.3) && $NontransferableTypes
// CHECK-NEXT: public func returnsOpaqueNT() -> some ~Transferable
// CHECK-NOT:  #if
// CHECK:      #endif
public func returnsOpaqueNT() -> some ~Transferable { NT() }

// Declarations that don't mention Transferable aren't guarded.
// CHECK-NOT:  #if
// CHECK:      public func plain<T>(_ t: T)
public func plain<T>(_ t: T) {}
