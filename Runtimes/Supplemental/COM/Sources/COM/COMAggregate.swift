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

#if $_MicrosoftCOM /* || $_XPCOM */

/// A COM implementation whose identity and lifetime can be controlled by a
/// controlling `IUnknown`.
///
/// When `controller` is non-`nil`, `QueryInterface`, `AddRef`, and `Release`
/// are forwarded to the controlling `IUnknown`. When it is `nil`, the
/// implementation behaves as a standalone COM object.
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
  /// The controlling `IUnknown`, or `nil` if the object is standalone.
  var controller: (any IUnknown)? { borrow }
}

#endif
