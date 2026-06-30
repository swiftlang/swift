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

/// A protocol that opts a `@com` class into COM aggregation.
///
/// When the compiler sees a `@com` class conforming to `COMAggregatable`, it
/// emits delegating variants of the three `IUnknown` vtable slots
/// (`AggregatedQueryInterface`, `AggregatedAddRef`, `AggregatedRelease`)
/// instead of the non-delegating versions. The delegating variants read the
/// `controller` property and forward all `IUnknown` operations to the outer
/// object's `IUnknown`. Classes that do not conform get the non-delegating
/// fast path with zero overhead.
///
/// This is a compile-time vtable slot selection — the only compiler awareness
/// needed is checking `COMAggregatable` conformance at vtable emission time.
///
/// Conformance is typically added by the `@COMAggregation` macro on the
/// generated forwarding class. Direct conformance is permitted for advanced
/// use cases.
///
/// ```swift
/// @com
/// final class ForwarderImpl: IAccessible, COMAggregatable {
///     let controller: (any IUnknown)?
///     private let inner: any IAccessible
///
///     var role: Int32 { get throws { try inner.role } }
/// }
/// ```
public protocol COMAggregatable: AnyObject {
  /// The outer object's `IUnknown`, or `nil` if the object is not aggregated.
  var controller: (any IUnknown)? { get }
}
