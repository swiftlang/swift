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
/// The compiler synthesizes this conformance for `@com` protocol declarations.
/// This allows generic code to recover the exact interface ID from a
/// requirement such as `Interface.Type: COMInterface` without making conforming
/// implementations carry that identity.
public protocol COMInterface {
  /// The IID of this interface.
  var IID: IID { get }
}
