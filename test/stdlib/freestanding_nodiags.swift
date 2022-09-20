// RUN: %target-swift-frontend                \
// RUN:     -concurrency-model=task-to-thread \
// RUN:     -parse-as-library                 \
// RUN:     -parse-stdlib                     \
// RUN:     -typecheck                        \
// RUN:     -verify %s

// REQUIRES: freestanding

import _Concurrency

@available(*, unavailable, message: "")
@globalActor // fine because unavailable
final class Hoo {
  actor Impl {}
  typealias ActorType = Impl
  static var shared: Impl { Impl() }
}

@main struct Main {
  @available(*, unavailable, message: "")
  static func main() async throws {} // fine because unavailable

  static func main() throws {} // fine because unavailable
}

@available(*, unavailable, message: "")
@MainActor(unsafe) // fine because unavailable
func chowMein() async {}

@available(*, unavailable, message: "")
@MainActor // fine because unavailable
class ChowMein {}

@available(*, unavailable, message: "")
func foo2(
    body: @MainActor @Sendable () throws -> () // fine because unavailable
) {}

@available(*, unavailable, message: "")
func foo3(
    body: @Hoo @Sendable () throws -> () // fine because unavailable
) {}
