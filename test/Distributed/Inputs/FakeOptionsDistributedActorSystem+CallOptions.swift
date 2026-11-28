//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0
//
// See LICENSE.txt for license information
// See CONTRIBUTORS.txt for the list of Swift project authors
//
// SPDX-License-Identifier: Apache-2.0
//
//===----------------------------------------------------------------------===//

/// The call options a `FakeOptionsActorSystem` proxy can carry in its ID.

// ==== ------------------------------------------------------------------------
// MARK: Call options

@available(SwiftStdlib 5.7, *)
public struct FakeCallOptions: Sendable, CustomStringConvertible {

  /// How many times a call makes another connection attempt after one fails.
  ///
  /// Only attempts made before the call is sent are retried, so a retry never
  /// executes a call twice. Calls without options make a single attempt.
  public var automaticRetries: Int = 0
  /// Free-form tag, to tell proxies apart in logs.
  public var label: String? = nil
  /// Called inline at each transition of a call made through the proxy.
  public var onEvent: (@Sendable (FakeCallEvent) -> Void)? = nil

  public init() {}

  public var description: String {
    "[automaticRetries: \(automaticRetries), label: \(label ?? "nil"), onEvent: \(onEvent == nil ? "nil" : "set")]"
  }
}

@available(SwiftStdlib 5.7, *)
public struct FakeCallEvent: Sendable, CustomStringConvertible {
  public enum Kind: Sendable {
    /// A connection attempt failed and the call makes attempt number `attempt`.
    case retrying(attempt: Int)
    case sent
    case finished
  }

  /// Distinguishes concurrent calls made through the same proxy.
  public let callID: Int
  public let kind: Kind

  public var description: String {
    switch kind {
    case .retrying(let attempt): return "event #\(callID) retrying(attempt \(attempt))"
    case .sent: return "event #\(callID) sent"
    case .finished: return "event #\(callID) finished"
    }
  }
}
