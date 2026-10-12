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
/// Following the three `IUnknown` entries, the interface provides two getters:
/// `object` at vtable slot 3 returns the Swift heap object, and `metadata` at
/// slot 4 returns that object's dynamic class metadata. Both pointers are
/// borrowed and must describe the same object. The queried interface reference
/// keeps the object alive while the runtime uses them.
///
/// A successful `QueryInterface` returns an owned interface reference. The
/// runtime releases it after using the borrowed identity; a Swift reference
/// returned to the caller must acquire independent ownership.
///
/// Consumers query this interface using its reserved IID. They must not assume
/// that the incoming interface, the identity interface, and the Swift object
/// have the same address, or inspect a foreign object's allocation prefix.
@com(interface: "8E369447-5188-5ADA-B9EC-8FCB732D226B")
public protocol ISwiftObject {
  var object: UnsafeMutableRawPointer { get }
  var metadata: UnsafeRawPointer { get }
}

extension ISwiftObject {
  @_transparent
  public var object: UnsafeMutableRawPointer {
    unsafeBitCast(self, to: UnsafeMutableRawPointer.self)
  }

  @_transparent
  public var metadata: UnsafeRawPointer {
    unsafeBitCast(type(of: self), to: UnsafeRawPointer.self)
  }
}
