// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency

enum Boom: Error {
  case bang
}

@available(SwiftStdlib 6.2, *)
func tests() async {
  // Task.init

  Task {
    42
  }

  Task { () throws -> Int in // expected-warning {{result of 'Task<Success, Failure>' initializer is unused}}
    if Bool.random() { throw Boom.bang }
    return 42
  }

  Task { () throws(Boom) -> Int in // expected-warning {{result of 'Task<Success, Failure>' initializer is unused}}
    if Bool.random() { throw Boom.bang }
    return 42
  }

  // Task.detached

  Task.detached {
    42
  }

  Task.detached { () throws -> Int in // expected-warning {{result of call to 'detached(name:priority:operation:)' is unused}}
    if Bool.random() { throw Boom.bang }
    return 42
  }

  Task.detached { () throws(Boom) -> Int in // expected-warning {{result of call to 'detached(name:priority:operation:)' is unused}}
    if Bool.random() { throw Boom.bang }
    return 42
  }

  // Task.immediate

  Task.immediate {
    42
  }

  Task.immediate { () throws -> Int in // expected-warning {{result of call to 'immediate(name:priority:executorPreference:operation:)' is unused}}
    if Bool.random() { throw Boom.bang }
    return 42
  }

  Task.immediate { () throws(Boom) -> Int in // expected-warning {{result of call to 'immediate(name:priority:executorPreference:operation:)' is unused}}
    if Bool.random() { throw Boom.bang }
    return 42
  }

  // Task.immediateDetached

  Task.immediateDetached {
    42
  }

  Task.immediateDetached { () throws -> Int in // expected-warning {{result of call to 'immediateDetached(name:priority:executorPreference:operation:)' is unused}}
    if Bool.random() { throw Boom.bang }
    return 42
  }

  Task.immediateDetached { () throws(Boom) -> Int in // expected-warning {{result of call to 'immediateDetached(name:priority:executorPreference:operation:)' is unused}}
    if Bool.random() { throw Boom.bang }
    return 42
  }
}
