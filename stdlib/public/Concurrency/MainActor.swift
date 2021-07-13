//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// A singleton actor whose executor is equivalent to the main
/// dispatch queue.
@available(SwiftStdlib 5.5, *)
@globalActor public final actor MainActor: GlobalActor {
  public static let shared = MainActor()

  @inlinable
  public nonisolated var unownedExecutor: UnownedSerialExecutor {
    #if compiler(>=5.5) && $BuiltinBuildMainExecutor
    return UnownedSerialExecutor(Builtin.buildMainActorExecutorRef())
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

  @inlinable
  public static var sharedUnownedExecutor: UnownedSerialExecutor {
    #if compiler(>=5.5) && $BuiltinBuildMainExecutor
    return UnownedSerialExecutor(Builtin.buildMainActorExecutorRef())
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

  @inlinable
  public nonisolated func enqueue(_ job: UnownedJob) {
    _enqueueOnMain(job)
  }
}

@available(SwiftStdlib 5.5, *)
extension MainActor {
  /// Execute the given body closure on the main actor.
  public static func run<T>(
    resultType: T.Type = T.self,
    body: @MainActor @Sendable () throws -> T
  ) async rethrows -> T {
    @MainActor func runOnMain(body: @MainActor @Sendable () throws -> T) async rethrows -> T {
      return try body()
    }

    return try await runOnMain(body: body)
  }
}
