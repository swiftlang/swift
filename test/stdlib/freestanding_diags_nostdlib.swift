// RUN: %target-swift-frontend                \
// RUN:     -concurrency-model=task-to-thread \
// RUN:     -typecheck                        \
// RUN:     -verify %s

// REQUIRES: freestanding

import _Concurrency

actor Simple {}

@globalActor // expected-error{{not permitted within task-to-thread concurrency model}}
class Goo {
  typealias ActorType = Simple

  static var shared : Simple { fatalError() }
}

@main struct Main {
  static func main() async throws {} // expected-error{{not permitted within task-to-thread concurrency model}}
}

