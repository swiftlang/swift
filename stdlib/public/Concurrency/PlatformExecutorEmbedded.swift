//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 6.2, *)
public struct PlatformExecutorFactory: ExecutorFactory {
  public static let mainExecutor: any MainExecutor = EmbeddedMainExecutor()
  public static let defaultExecutor: any TaskExecutor = EmbeddedDefaultExecutor()
}

// .. Platform Main Executor ...................................................

@available(SwiftStdlib 6.2, *)
public final class EmbeddedMainExecutor: MainExecutor, @unchecked Sendable {

  public init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
  }

  // We can't implement enqueue<C: Clock> in Embedde Swift because we aren't
  // allowed to have generics in an existential there.

  public func run() throws {
  }

  public func run(until condition: () -> Bool) throws {
  }

  public func stop() {
  }

  public func registerEvent(handler: @escaping () -> ()) -> ExecutorEvent {
    return ExecutorEvent(id: 0)
  }

  public func deregister(event: ExecutorEvent) {
  }

  public func notify(event: ExecutorEvent) {
  }

}

// .. Platform Task Executor ...................................................

@available(SwiftStdlib 6.2, *)
public final class EmbeddedDefaultExecutor: TaskExecutor, @unchecked Sendable {

  public init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
  }

}
