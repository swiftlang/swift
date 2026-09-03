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

#if $_MicrosoftCOM

public import _SwiftCOMShims

extension ObjectIdentifier {
  /// Creates an identifier for the COM identity of an interface.
  ///
  /// COM defines object identity as the interface pointer returned by
  /// `QueryInterface` for `IUnknown`. Different interfaces on the same object
  /// therefore produce the same identifier even when their own interface
  /// pointers differ.
  @_alwaysEmitIntoClient
  public init<Interface>(_ interface: borrowing Interface)
      where Interface.Type: COMInterface {
    let pUnk = ManagedObject<Interface>.passUnretained(interface)
    let vtable = pUnk.load(as: UnsafePointer<UnsafeRawPointer>.self)
    let queryInterface =
      unsafeBitCast(vtable[0], to: _SwiftCOMQueryInterfaceFunction.self)

    var identity: UnsafeMutableRawPointer?
    let hr = withUnsafePointer(to: IUnknown.IID) { iid in
      queryInterface(pUnk, UnsafeRawPointer(iid), &identity)
    }
    guard hr >= 0, let identity else {
      preconditionFailure("QueryInterface for IUnknown failed")
    }

    self = unsafeBitCast(identity, to: ObjectIdentifier.self)

    let identityVTable =
      identity.load(as: UnsafePointer<UnsafeRawPointer>.self)
    let release =
      unsafeBitCast(identityVTable[2], to: _SwiftCOMLifetimeFunction.self)
    _ = release(identity)
  }
}

/// Returns whether two COM interfaces identify the same object.
@_alwaysEmitIntoClient
public func === <Left, Right>(_ lhs: borrowing Left,
                              _ rhs: borrowing Right) -> Bool
    where Left.Type: COMInterface, Right.Type: COMInterface {
  return ObjectIdentifier(lhs) == ObjectIdentifier(rhs)
}

/// Returns whether two optional COM interfaces identify the same object.
@_alwaysEmitIntoClient
public func === <Left, Right>(_ lhs: Left?, _ rhs: Right?) -> Bool
    where Left.Type: COMInterface, Right.Type: COMInterface {
  guard let lhs else {
    return rhs == nil
  }
  guard let rhs else {
    return false
  }
  return lhs === rhs
}

/// Returns whether two COM interfaces identify different objects.
@_alwaysEmitIntoClient
public func !== <Left, Right>(_ lhs: borrowing Left,
                              _ rhs: borrowing Right) -> Bool
    where Left.Type: COMInterface, Right.Type: COMInterface {
  return !(lhs === rhs)
}

/// Returns whether two optional COM interfaces identify different objects.
@_alwaysEmitIntoClient
public func !== <Left, Right>(_ lhs: Left?, _ rhs: Right?) -> Bool
    where Left.Type: COMInterface, Right.Type: COMInterface {
  return !(lhs === rhs)
}

#endif // $_MicrosoftCOM
