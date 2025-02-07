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

// ###TODO: Flesh this file out

// .. Platform Main Executor ...................................................

@available(SwiftStdlib 6.2, *)
public final class PlatformMainExecutor: MainExecutor, @unchecked Sendable {

  public func enqueue(_ job: consuming ExecutorJob) {
  }

  // We can't implement enqueue<C: Clock> in Embedde Swift because we aren't
  // allowed to have generics in an existential there.

  public var supportsScheduling: Bool { false }

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
public final class PlatformDefaultExecutor: TaskExecutor, @unchecked Sendable {

  public func enqueue(_ job: consuming ExecutorJob) {
  }

  // We can't implement enqueue<C: Clock> in Embedde Swift because we aren't
  // allowed to have generics in an existential there.

  public var supportsScheduling: Bool { false }

}
