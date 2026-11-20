// rdar://167871796
// RUN: %target-typecheck-verify-swift

class Object {}

struct LogInterpolation: StringInterpolationProtocol {
  init(literalCapacity: Int, interpolationCount: Int) {}
  mutating func appendLiteral(_ literal: String) {}
  mutating func appendInterpolation(_ object: @autoclosure @escaping () -> Object?) {}
  mutating func appendInterpolation(_ string: @autoclosure @escaping () -> String) {}
}

struct LogMessage: ExpressibleByStringInterpolation {
  init(stringLiteral value: String) {}
  init(stringInterpolation: LogInterpolation) {}
}

struct Logger {
  func debug(_ message: LogMessage) {}
}

class Status {
  func summary() -> String { "" }
}

func lockContent(completion: @escaping (Bool, Status?) -> Void) {}

func test(logger: Logger) {
  // expected-error@+1 {{failed to produce diagnostic for expression; please submit a bug report}}
  lockContent(completion: { contentLocked, newerInProgress in
    if newerInProgress == nil {
      logger.debug("No newer version")
    } else {
      logger.debug("Newer version in progress \(newerInProgress.summary)")
    }
  })
}

func f(_ object: Object?) {}
func f(_ string: String) {}

func test(status: Status?) {
  f(status.summary) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
