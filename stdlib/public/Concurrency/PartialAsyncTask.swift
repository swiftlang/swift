//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift
@_implementationOnly import _SwiftConcurrencyShims

/// A partial task is a unit of scheduleable work.
public struct PartialAsyncTask {
  private var context: UnsafeMutablePointer<_SwiftContext>

  public func run() { }
}


public struct UnsafeContinuation<T> {
  private var context: UnsafeRawPointer

  public func resume(_: T) { }
}

public struct UnsafeThrowingContinuation<T> {
  private var context: UnsafeRawPointer

  public func resume(_: T) { }
  public func fail(_: Error) { }
}


