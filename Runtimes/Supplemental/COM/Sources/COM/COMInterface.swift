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

/// Marks a Swift protocol as a COM interface with a stable interface ID.
///
/// The compiler makes the metatype of every `@com` protocol conform to
/// `COMInterface`. This allows generic code to recover the IID from a
/// requirement such as `Interface.Type: COMInterface` without making conforming
/// implementation types carry that identity.
///
/// This conformance is compiler-managed and cannot be written explicitly.
public protocol COMInterface {
  /// The IID of this interface.
  var IID: IID { get }
}
