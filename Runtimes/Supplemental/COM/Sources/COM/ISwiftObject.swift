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

/// The Swift UUID Namespace
///
/// IIDs for Swift-specific COM interfaces are derived deterministically using
/// UUID version 5 (SHA-1, RFC 4122 Section 4.3) from a reserved Swift namespace
/// UUID:
///
/// ```
/// Namespace: {E29CA80E-0000-0000-C000-000000000000}
/// ```
///
/// To derive an IID, the namespace bytes are concatenated with the UTF-8
/// encoding of the interface name, SHA-1 hashed, and the first 16 bytes are
/// formatted as a UUID v5 (version nibble set to 5, variant bits set to RFC
/// 4122). This produces a stable, deterministic IID for each name within the
/// namespace, with no external registry needed.

/// A COM interface for recovering the underlying Swift heap object from a COM
/// interface pointer.
///
/// `ISwiftObject` is synthesised implicitly on every `@com` class. The runtime
/// uses it to implement `as?` casting from COM existentials to concrete Swift
/// types. Explicitly conforming a `@com` class to `ISwiftObject` is a
/// compile-time error.
///
/// The interface has a single method (`object`, at vtable slot 3) that returns
/// a borrowed reference to the Swift heap object. Like `IUnknown`'s methods,
/// this is not exposed as a Swift protocol requirement because the
/// implementation is compiler-managed and sealed.
///
/// The `object` getter recovers the Swift object pointer using the non-virtual
/// thunk adjustment stored at `vtable[−1]`. Each COM interface on an object has
/// its own vtable, and each vtable carries its own adjustment value at this
/// negative offset. The adjustment records the byte distance from the COM
/// interface pointer to the Swift object pointer. This is analogous to the
/// Itanium C++ ABI's `offset_to_top` field in structure — a per-vtable fixed
/// displacement stored at a negative index — though the direction is reversed:
/// rather than retreating to the lowest address of the complete object, the
/// adjustment advances forward to where the Swift heap object begins within the
/// allocation. The adjustment is per-vtable because each interface's vtable
/// pointer sits at a different offset within the object's COM block.
///
/// A user-provided implementation would bypass this per-vtable adjustment and
/// produce incorrect results. The runtime calls `object` only after
/// `QueryInterface` for `ISwiftObject` has confirmed the object is
/// Swift-originated, so the adjustment is never read speculatively on foreign
/// COM objects.
@com(interface: "8E369447-5188-5ADA-B9EC-8FCB732D226B")
public protocol ISwiftObject: IUnknown {
  var object: AnyObject { get }
  var metadata: UnsafeRawPointer { get }
}

extension ISwiftObject {
  var object: AnyObject {
    self
  }

  var metadata: UnsafeRawPointer {
    unsafeBitCast(type(of: self), to: UnsafeRawPointer.self)
  }
}
