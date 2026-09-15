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

/// Provides interfaces that are not declared directly by a COM implementation.
///
/// `QueryInterface` checks the implementation's declared interfaces before
/// consulting this protocol. Use it for conditional interfaces and tear-offs.
public protocol COMInterfaceResolver {
  /// Return an owned interface reference for `iid`, or `nil` when the
  /// interface is unsupported.
  func resolve(_ iid: borrowing IID) -> COMInterfaceResolution?
}
