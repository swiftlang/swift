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

public import WinSDK
public import _SwiftCOMShims

// MARK: - Helpers

extension Unmanaged where Instance == AnyObject {
  /// Recovers the Swift object pointer `P` and the heap object from a COM
  /// interface pointer via the `vtable[-1]` byte adjustment.
  @_transparent
  @usableFromInline
  internal static func from(unsafeCOMPointer pUnk: UnsafeMutableRawPointer) -> Self {
    let vtable = pUnk.load(as: UnsafePointer<UnsafeRawPointer>.self)
    let object = pUnk.advanced(by: Int(bitPattern: vtable.advanced(by: -1).pointee))
    return Unmanaged<AnyObject>.fromOpaque(object)
  }
}

// MARK: - Aggregated IUnknown Methods

/// Aggregated `QueryInterface`.
///
/// Forwards the operation to the controlling `IUnknown` when the receiver is
/// aggregated, or queries the receiver directly when it is standalone.
@implementation @c
@_alwaysEmitIntoClient
public func AggregatedQueryInterface(_ pUnk: UnsafeMutableRawPointer, _ riid: UnsafeRawPointer,
                                     _ ppvObject: UnsafeMutablePointer<UnsafeMutableRawPointer?>)
    -> HRESULT {
  let aggregate = Unmanaged<AnyObject>.from(unsafeCOMPointer: pUnk).takeUnretainedValue() as! COMAggregatable
  guard let controller = aggregate.controller else {
    return QueryInterface(pUnk, riid, ppvObject)
  }

  let outer = ManagedObject<IUnknown>.passUnretained(controller)
  let vtable = outer.load(as: UnsafePointer<UnsafeRawPointer>.self)
  let operation = unsafeBitCast(vtable[0],
                                to: _SwiftCOMQueryInterfaceFunction.self)
  return operation(outer, riid, ppvObject)
}

/// Aggregated `AddRef`.
///
/// Forwards the operation to the controlling `IUnknown` when the receiver is
/// aggregated, or calls the receiver's `AddRef` directly when it is standalone.
@implementation @c
@_alwaysEmitIntoClient
public func AggregatedAddRef(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  let aggregate = Unmanaged<AnyObject>.from(unsafeCOMPointer: pUnk).takeUnretainedValue() as! COMAggregatable
  guard let controller = aggregate.controller else {
    return AddRef(pUnk)
  }

  let outer = ManagedObject<IUnknown>.passUnretained(controller)
  let vtable = outer.load(as: UnsafePointer<UnsafeRawPointer>.self)
  return unsafeBitCast(vtable[1], to: _SwiftCOMLifetimeFunction.self)(outer)
}

/// Aggregated `Release`.
///
/// Forwards the operation to the controlling `IUnknown` when the receiver is
/// aggregated, or calls the receiver's `Release` directly when it is standalone.
@implementation @c
@_alwaysEmitIntoClient
public func AggregatedRelease(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  let aggregate = Unmanaged<AnyObject>.from(unsafeCOMPointer: pUnk).takeUnretainedValue() as! COMAggregatable
  guard let controller = aggregate.controller else {
    return Release(pUnk)
  }

  let outer = ManagedObject<IUnknown>.passUnretained(controller)
  let vtable = outer.load(as: UnsafePointer<UnsafeRawPointer>.self)
  return unsafeBitCast(vtable[2], to: _SwiftCOMLifetimeFunction.self)(outer)
}

#endif
