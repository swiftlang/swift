// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s \
// RUN:     -swift-version 5 \
// RUN:     -enable-library-evolution \
// RUN:     -emit-module -module-name Swift -parse-stdlib \
// RUN:     -o %t/Swift.swiftmodule \
// RUN:     -emit-module-interface-path %t/Swift.swiftinterface

// RUN: %FileCheck %s < %t/Swift.swiftinterface
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/Swift.swiftinterface -module-name Swift -parse-stdlib

// Compilers without the Deinitable protocol can't resolve `~Deinitable`, so
// every declaration that suppresses it in its generic signature must be
// hidden from them (rdar://191428783).

@_marker public protocol Escapable {}
@_marker public protocol Deinitable {}
@_marker public protocol Copyable: Deinitable {}

// CHECK:      @_marker public protocol Copyable {
// CHECK-NEXT: }
// CHECK-NEXT: #endif
// CHECK-NEXT: #if compiler(>=5.3) && $DeinitableProtocol
// CHECK-NEXT: public func takeNondeinitable<T>(_ t: borrowing T) where T : ~Copyable, T : ~Deinitable
// CHECK-NEXT: #endif
public func takeNondeinitable<T: ~Copyable & ~Deinitable>(_ t: borrowing T) {}

// CHECK-NOT:  #if
// CHECK:      public func takeNoncopyable<T>(_ t: borrowing T) where T : ~Copyable
public func takeNoncopyable<T: ~Copyable>(_ t: borrowing T) {}

// CHECK:      #if compiler(>=5.3) && $DeinitableProtocol
// CHECK-NEXT: public func takeSomeNondeinitable(_ t: borrowing some ~Copyable & ~Deinitable)
// CHECK-NEXT: #endif
public func takeSomeNondeinitable(_ t: borrowing some ~Copyable & ~Deinitable) {}

// The members of a guarded type aren't guarded again.
// CHECK:      #if compiler(>=5.3) && $DeinitableProtocol
// CHECK-NEXT: public struct Box<T> : ~Swift::Copyable where T : ~Copyable, T : ~Deinitable {
// CHECK-NEXT:   public func method()
// CHECK-NEXT: }
// CHECK-NEXT: #endif
public struct Box<T: ~Copyable & ~Deinitable>: ~Copyable {
  public func method() {}
}

// CHECK:      #if compiler(>=5.3) && $DeinitableProtocol
// CHECK-NEXT: extension Swift::Box where T : ~Copyable, T : ~Deinitable {
// CHECK-NEXT:   public func extensionMethod()
// CHECK-NEXT: }
// CHECK-NEXT: #endif
extension Box where T: ~Copyable & ~Deinitable {
  public func extensionMethod() {}
}

public struct Container {
  // CHECK:      public struct Container {
  // CHECK-NEXT:   #if compiler(>=5.3) && $DeinitableProtocol
  // CHECK-NEXT:   public func method<T>(_ t: borrowing T) where T : ~Copyable, T : ~Deinitable
  // CHECK-NEXT:   #endif
  // CHECK-NEXT: }
  public func method<T: ~Copyable & ~Deinitable>(_ t: borrowing T) {}
}

public protocol P: ~Copyable {}

// An unconstrained extension of a guarded type is guarded too, even though the
// generic parameter is Deinitable in the extension's signature.
// CHECK:      #if compiler(>=5.3) && $DeinitableProtocol
// CHECK-NEXT: extension Swift::Box {
// CHECK-NEXT:   public func unconstrainedExtensionMethod()
// CHECK-NEXT: }
// CHECK-NEXT: #endif
extension Box {
  public func unconstrainedExtensionMethod() {}
}

// CHECK:      #if compiler(>=5.3) && $DeinitableProtocol
// CHECK-NEXT: extension Swift::Box : Swift::P {
// CHECK-NEXT: }
// CHECK-NEXT: #endif
extension Box: P {}

// CHECK:      #if compiler(>=5.3) && $DeinitableProtocol
// CHECK-NEXT: public func opaqueResult() -> some ~Copyable & ~Deinitable
// CHECK-EMPTY:
// CHECK-NEXT: #endif
public func opaqueResult() -> some ~Copyable & ~Deinitable { Container() }

// The requirements of a protocol that suppresses Deinitable aren't guarded
// separately, so compilers without the feature still see them.
// CHECK:      #if compiler(>=5.3) && $DeinitableProtocol
// CHECK-NEXT: public protocol SuppressesDeinitable : ~Copyable, ~Deinitable {
// CHECK-NEXT:   func requirement()
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: public protocol SuppressesDeinitable : ~Copyable {
// CHECK-NEXT:   func requirement()
// CHECK-NEXT: }
// CHECK-NEXT: #endif
public protocol SuppressesDeinitable: ~Copyable, ~Deinitable {
  func requirement()
}

// CHECK:      #if compiler(>=5.3) && $DeinitableProtocol
// CHECK-NEXT: public protocol HasNondeinitableAssociatedType {
// CHECK-NEXT:   associatedtype A : ~Copyable, ~Deinitable
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: public protocol HasNondeinitableAssociatedType {
// CHECK-NEXT:   associatedtype A : ~Copyable
// CHECK-NEXT: }
// CHECK-NEXT: #endif
public protocol HasNondeinitableAssociatedType {
  associatedtype A: ~Copyable, ~Deinitable
}
