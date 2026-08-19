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

/// Identifies a remote machine for DCOM activation.
///
/// Passed to `withActivationContext(_:server:operation:)` when activating COM
/// objects on a remote machine via `CLSCTX.remote`. Wraps the information
/// needed by the underlying `CoCreateInstanceEx` call.
///
/// ```swift
/// try await withActivationContext(.remote, server: COMServerInfo(name: "host.example.com")) {
///     let engine = try CImplementation()
/// }
/// ```
public struct COMServerInfo {
  /// The hostname or IP address of the remote machine.
  public var name: String

  /// Creates a server info targeting the specified host.
  ///
  /// - Parameter name: The hostname or IP address of the remote DCOM server.
  public init(name: String) {
    self.name = name
  }
}

extension COMServerInfo: Sendable {
}
