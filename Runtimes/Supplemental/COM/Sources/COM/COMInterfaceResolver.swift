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

/// An extension point for `QueryInterface` beyond a class's declared `@com`
/// conformances.
///
/// The synthesised `QueryInterface` checks `COMAggregatable` (forwarding to the
/// controlling unknown if present), then `ISwiftObject`, then `IUnknown`, then
/// scans the class's static conformance table. If no match is found and the
/// class conforms to `COMInterfaceResolver`, the `resolve(_:)` method is
/// called as a final step before returning `E_NOINTERFACE`.
///
/// This protocol enables COM patterns that add interfaces dynamically at
/// runtime:
///
/// - **Aggregation**: the `@COMAggregation` macro synthesises a
///   `COMInterfaceResolver` conformance that delegates to inner objects for
///   aggregated interfaces.
/// - **Cached tear-offs** (`CachedTearOff<T>`): create an interface
///   implementation on first query and return the cached instance on subsequent
///   queries.
/// - **Disposable tear-offs** (`DisposableTearOff<T>`): create a fresh,
///   independently reference-counted implementation on every query.
/// - **Conditional interfaces**: return a pointer for an IID only when some
///   runtime condition is met.
///
/// Classes that do not conform to `COMInterfaceResolver` pay no cost for this
/// extension point; the synthesised `QueryInterface` skips the callback
/// entirely.
public protocol COMInterfaceResolver {
  /// Called by the synthesised `QueryInterface` for IIDs not found in the
  /// static conformance table.
  ///
  /// Return a valid COM interface pointer for the requested IID, or `nil` to
  /// decline.  If a non-nil pointer is returned, `QueryInterface` will `AddRef`
  /// it before returning it to the caller.
  ///
  /// - Parameter iid: The interface identifier being queried.
  /// - Returns: A COM interface pointer, or `nil` if this resolver does not
  ///            handle the IID.
  func resolve(_ iid: borrowing IID) -> UnsafeMutableRawPointer?
}
