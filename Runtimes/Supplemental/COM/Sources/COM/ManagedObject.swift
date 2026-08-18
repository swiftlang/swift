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

private import Builtin

/// Converts between a COM interface reference and its ABI pointer.
///
/// Use `takeRetainedValue(_:)` to adopt a "+1" interface pointer returned by a
/// COM operation. Use `passUnretained(_:)` to borrow the pointer represented by
/// an existing interface reference without changing its reference count.
@unsafe
public enum ManagedObject<Interface> where Interface.Type: COMInterface {
  /// Adopts a "+1" COM interface pointer as a managed interface reference.
  @_transparent
  public static func takeRetainedValue(_ pointer: UnsafeMutableRawPointer)
      -> Interface {
    return Builtin.takeFromRawPointer(pointer._rawValue)
  }

  /// Adopts an optional "+1" COM interface pointer.
  @_transparent
  public static func takeRetainedValue(_ pointer: UnsafeMutableRawPointer?)
      -> Interface? {
    guard let pointer else { return nil }
    return takeRetainedValue(pointer)
  }

  /// Adopts an optional typed "+1" COM interface pointer.
  @_transparent
  public static func takeRetainedValue<Pointee>(_ pointer: UnsafeMutablePointer<Pointee>?)
      -> Interface? {
    guard let pointer else { return nil }
    return takeRetainedValue(UnsafeMutableRawPointer(pointer))
  }

  /// Borrows the ABI pointer represented by a COM interface reference.
  @_transparent
  public static func passUnretained(_ interface: borrowing Interface)
      -> UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(Builtin.bridgeToRawPointer(interface))
  }
}
