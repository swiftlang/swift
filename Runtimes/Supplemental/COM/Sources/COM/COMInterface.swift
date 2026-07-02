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


/// Marks a Swift protocol as a COM interface with a stable interface ID.
///
/// `COMInterface` is adopted by COM-facing protocols to expose the interface's
/// `IID`, which is used for `QueryInterface` and cast-based interface
/// discovery. `@com` protocol declarations synthesize this conformance.
public protocol COMInterface {
  /// The IID of this interface.
  static var iid: IID { get }
}
