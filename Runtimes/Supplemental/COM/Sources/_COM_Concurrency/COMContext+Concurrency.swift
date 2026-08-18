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

#if $_MicrosoftCOM

public import COM
public import _Concurrency

extension COMActivationOptions {
  /// The activation options inherited by the current task and its child tasks.
  @TaskLocal
  public static var current: COMActivationOptions = .default
}

/// Runs an asynchronous operation in the process multithreaded apartment.
///
/// An asynchronous task may resume on a different worker thread, so this
/// overload cannot establish a thread-bound single-threaded apartment. Use the
/// synchronous overload from `COM` for an apartment-threaded context.
// TODO(compnerd): use `~Copyable` for `Result` when `withValue` permits it.
@_alwaysEmitIntoClient
public func withCOMContext<Result>(activation: CLSCTX = .all,
                                   server: consuming COMServerInfo? = nil,
                                   _ body: () async throws -> sending Result)
    async throws -> sending Result {
  let activation = COMActivationOptions(context: activation, server: server)
  return try await COMActivationOptions.$current.withValue(activation) {
    let context = try COMMultiThreadedContext()
    defer { withExtendedLifetime(context) { } }
    return try await body()
  }
}

#endif // $_MicrosoftCOM
