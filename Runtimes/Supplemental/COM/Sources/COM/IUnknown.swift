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

#if os(Windows)
public import WinSDK
#endif

/// The root interface for all COM objects.
///
/// Every COM interface derives from `IUnknown`, which provides the three
/// fundamental operations of the COM binary interface: `QueryInterface`
/// (interface discovery), `AddRef` (reference count increment), and `Release`
/// (reference count decrement).
///
/// In Swift, these three operations are compiler-managed and sealed:
///
/// - `QueryInterface` is expressed as Swift's `as?` operator. Casting between
///   `@com` protocol existentials generates a `QueryInterface` call with the
///   target protocol's IID.
/// - `AddRef` and `Release` are managed by Swift ARC through the unified
///   reference count.  Swift developers never call them directly.
///
/// The protocol body is empty because the three COM methods cannot be expressed
/// as ordinary Swift protocol requirements. They occupy vtable slots 0–2 but
/// are sealed: `AddRef`/`Release` must go through ARC (manual calls would
/// corrupt the refcount), and `QueryInterface` must go through `as?` (which
/// handles existential wrapping and the `ISwiftObject` recovery path). Allowing
/// override of any of the three would break the COM identity rule, QI symmetry,
/// or the unified refcount contract.
///
/// For advanced use cases that require raw `QueryInterface` access (e.g.,
/// dynamic IIDs not known at compile time), the underlying C functions remain
/// available through `WinSDK`.
///
/// `IUnknown` conformance is implied automatically on every `@com` class.
/// Writing `: IUnknown` explicitly is allowed but unnecessary.
///
/// The IID `{00000000-0000-0000-C000-000000000046}` is the well-known
/// identifier assigned to `IUnknown` by the COM specification. `QueryInterface`
/// for this IID returns the primary interface pointer, satisfying the COM
/// identity rule.
@com(interface: "00000000-0000-0000-C000-000000000046")
public protocol IUnknown: AnyObject {
}

// MARK: - Helpers

/// Recovers the Swift object pointer `P` and the heap object from a COM
/// interface pointer via the `vtable[-1]` byte adjustment, then passes both
/// to `body`.
@inline(__always) @inlinable
internal func withHeapObject<T>(of pUnk: UnsafeMutableRawPointer,
                                _ body: (UnsafeMutableRawPointer, AnyObject) -> T) -> T {
  let vtable = pUnk.load(as: UnsafePointer<UnsafeRawPointer>.self)
  let P = pUnk.advanced(by: Int(bitPattern: vtable.advanced(by: -1).pointee))
  let object = Unmanaged<AnyObject>.fromOpaque(P).takeUnretainedValue()
  return body(P, object)
}

// MARK: - QueryInterface

/// Non-delegating `QueryInterface`.
///
/// Used by the compiler for `@com` classes that do not conform to
/// `COMAggregatable`. The `AnyObject` cast is deferred to the
/// `COMInterfaceResolver` fallback path; the `IUnknown` and `ISwiftObject`
/// fast paths are pure pointer arithmetic.
@_alwaysEmitIntoClient
public func QueryInterface(_ pUnk: UnsafeMutableRawPointer, _ riid: borrowing IID,
                           _ ppvObject: UnsafeMutablePointer<UnsafeMutableRawPointer?>,
                           conformances table: borrowing Span<IID>) -> HRESULT {
  let stride = MemoryLayout<UnsafeRawPointer>.stride

  // Recover P via vtable[-1]. No AnyObject cast on the fast path.
  let vtable = pUnk.load(as: UnsafePointer<UnsafeRawPointer>.self)
  let P = pUnk.advanced(by: Int(bitPattern: vtable.advanced(by: -1).pointee))

  // ISwiftObject fast path: always at P[-1]. This is the most common QI
  // in Swift — every `as?` to a concrete @com class goes through here.
  if riid == ISwiftObject.IID {
    ppvObject.pointee = P.advanced(by: -stride)
    _ = swift_retain(P)
    return S_OK
  }

  // IUnknown returns the primary interface pointer at P[-2].
  if riid == IUnknown.IID {
    ppvObject.pointee = P.advanced(by: -2 * stride)
    _ = swift_retain(P)
    return S_OK
  }

  // Walk the conformance table. Entry i maps to P[-(i + 2)].
  // ISwiftObject.IID serves as a sentinel: the fast path above already
  // handled the match case, so hitting it here terminates the scan.
  for offset in 0 ..< table.count {
    let iid = table[offset]

    guard iid == riid else {
      if iid == ISwiftObject.IID { break }
      continue
    }

    ppvObject.pointee = P.advanced(by: -(offset + 2) * stride)
    _ = swift_retain(P)
    return S_OK
  }

  // COMInterfaceResolver callback — AnyObject cast only on this fallback path.
  let object = Unmanaged<AnyObject>.fromOpaque(P).takeUnretainedValue()
  if let resolver = object as? COMInterfaceResolver,
     let pointer = resolver.resolve(riid) {
    ppvObject.pointee = pointer
    _ = swift_retain(P)
    return S_OK
  }

  ppvObject.pointee = nil
  return E_NOINTERFACE
}

/// Aggregated `QueryInterface`.
///
/// Used by the compiler for `@com` classes that conform to `COMAggregatable`.
/// Forwards the entire call to the controlling unknown's `QueryInterface`.
@_alwaysEmitIntoClient
public func AggregatedQueryInterface(_ pUnk: UnsafeMutableRawPointer, _ riid: borrowing IID,
                                     _ ppvObject: UnsafeMutablePointer<UnsafeMutableRawPointer?>,
                                     conformances table: borrowing Span<IID>) -> HRESULT {
  return withHeapObject(of: pUnk) { _, object in
    let aggregate = object as! COMAggregatable
    let outerPtr = UnsafeMutableRawPointer(unsafeCOMPointer: aggregate.controller!)
    return QueryInterface(outerPtr, riid, ppvObject, conformances: table)
  }
}

// MARK: - AddRef

/// Non-delegating `AddRef`.
///
/// Used by the compiler for `@com` classes that do not conform to
/// `COMAggregatable`. Recovers `P` via `vtable[-1]` and calls
/// `swift_retainReturningCount` directly. No dynamic checks.
@_alwaysEmitIntoClient
public func AddRef(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  let vtable = pUnk.load(as: UnsafePointer<UnsafeRawPointer>.self)
  let P = pUnk.advanced(by: Int(bitPattern: vtable.advanced(by: -1).pointee))
  return UInt32(truncatingIfNeeded: swift_retainReturningCount(P))
}

/// Aggregated `AddRef`.
///
/// Used by the compiler for `@com` classes that conform to `COMAggregatable`.
/// Forwards to the controlling unknown's `AddRef`.
@_alwaysEmitIntoClient
public func AggregatedAddRef(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  return withHeapObject(of: pUnk) { _, object in
    let aggregate = object as! COMAggregatable
    return AddRef(UnsafeMutableRawPointer(unsafeCOMPointer: aggregate.controller!))
  }
}

// MARK: - Release

/// Non-delegating `Release`.
///
/// Used by the compiler for `@com` classes that do not conform to
/// `COMAggregatable`. Recovers `P` via `vtable[-1]` and calls
/// `swift_releaseReturningCount` directly. No dynamic checks.
@_alwaysEmitIntoClient
public func Release(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  let vtable = pUnk.load(as: UnsafePointer<UnsafeRawPointer>.self)
  let P = pUnk.advanced(by: Int(bitPattern: vtable.advanced(by: -1).pointee))
  return UInt32(truncatingIfNeeded: swift_releaseReturningCount(P))
}

/// Aggregated `Release`.
///
/// Used by the compiler for `@com` classes that conform to `COMAggregatable`.
/// Forwards to the controlling unknown's `Release`.
@_alwaysEmitIntoClient
public func AggregatedRelease(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  return withHeapObject(of: pUnk) { _, object in
    let aggregate = object as! COMAggregatable
    return Release(UnsafeMutableRawPointer(unsafeCOMPointer: aggregate.controller!))
  }
}
