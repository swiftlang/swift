//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// An owned COM interface reference returned by a dynamic interface resolver.
///
/// A result owns exactly one reference to `pointer`. Destroying it invokes
/// `Release` through the interface's vtable. Consuming it through the compiler
/// hook transfers that reference to the `QueryInterface` caller.
@frozen
public struct COMInterfaceResolution: ~Copyable {
  @usableFromInline
  internal let pointer: UnsafeMutableRawPointer

  /// Creates an owned result by retaining a COM interface.
  ///
  /// - Parameter interface: The interface to retain.
  @_transparent
  public init<Interface>(_ interface: borrowing Interface)
      where Interface.Type: COMInterface {
    typealias AddRef = @convention(c) (UnsafeMutableRawPointer) -> UInt32

    let pointer = ManagedObject<Interface>.passUnretained(interface)
    let vtable = pointer.load(as: UnsafePointer<UnsafeRawPointer>.self)
    _ = unsafeBitCast(vtable[1], to: AddRef.self)(pointer)
    self.pointer = pointer
  }

  /// Creates a result by consuming an existing "+1" COM reference.
  ///
  /// The caller must ensure `pointer` is a valid COM interface pointer for
  /// which it owns exactly one reference.
  @_transparent
  public init(consuming pointer: UnsafeMutableRawPointer) {
    self.pointer = pointer
  }

  @_transparent
  deinit {
    typealias Release = @convention(c) (UnsafeMutableRawPointer) -> UInt32

    let vtable = pointer.load(as: UnsafePointer<UnsafeRawPointer>.self)
    _ = unsafeBitCast(vtable[2], to: Release.self)(pointer)
  }

  /// Transfers the owned reference out of this result.
  @usableFromInline
  @_transparent
  internal consuming func take() -> UnsafeMutableRawPointer {
    let pointer = self.pointer
    discard self
    return pointer
  }
}
