// RUN: %target-typecheck-verify-swift -diagnostics-editor-mode

typealias TimeInterval = Double

let NSEC_PER_USEC : UInt64 = 1000
let NSEC_PER_SEC : UInt64 = 1000000000

public enum TemporalProxy {
  case seconds(Int)
  case milliseconds(Int)
  case microseconds(Int)
  case nanoseconds(Int)
  @_downgrade_exhaustivity_check
  case never
}

func unproxify(t : TemporalProxy) -> TimeInterval {
  switch t { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 {{do you want to add missing cases?}}
  case let .seconds(s):
    return TimeInterval(s)
  case let .milliseconds(ms):
    return TimeInterval(TimeInterval(ms) / 1000.0)
  case let .microseconds(us):
    return TimeInterval( UInt64(us) * NSEC_PER_USEC ) / TimeInterval(NSEC_PER_SEC)
  case let .nanoseconds(ns):
    return TimeInterval(ns) / TimeInterval(NSEC_PER_SEC)
  }
}
