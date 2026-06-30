//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if $_MicrosoftCOM

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
/// Used by the compiler for `@com` classes that conform to `COMAggregatable`.
/// Forwards the entire call to the controlling unknown's `QueryInterface`.
@_alwaysEmitIntoClient
public func AggregatedQueryInterface(_ pUnk: UnsafeMutableRawPointer, _ riid: borrowing IID,
                                     _ ppvObject: UnsafeMutablePointer<UnsafeMutableRawPointer?>,
                                     conformances table: borrowing Span<IID>) -> HRESULT {
  let aggregate = Unmanaged<AnyObject>.from(unsafeCOMPointer: pUnk).takeUnretainedValue() as! COMAggregatable
  let outer = UnsafeMutableRawPointer(aggregate.controller!)
  return QueryInterface(outer, riid, ppvObject, conformances: table)
}

/// Aggregated `AddRef`.
///
/// Used by the compiler for `@com` classes that conform to `COMAggregatable`.
/// Forwards to the controlling unknown's `AddRef`.
@_alwaysEmitIntoClient
public func AggregatedAddRef(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  let aggregate = Unmanaged<AnyObject>.from(unsafeCOMPointer: pUnk).takeUnretainedValue() as! COMAggregatable
  return AddRef(UnsafeMutableRawPointer(aggregate.controller!))
}

/// Aggregated `Release`.
///
/// Used by the compiler for `@com` classes that conform to `COMAggregatable`.
/// Forwards to the controlling unknown's `Release`.
@_alwaysEmitIntoClient
public func AggregatedRelease(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  let aggregate = Unmanaged<AnyObject>.from(unsafeCOMPointer: pUnk).takeUnretainedValue() as! COMAggregatable
  return Release(UnsafeMutableRawPointer(aggregate.controller!))
}

#endif
