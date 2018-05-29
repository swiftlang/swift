// RUN: %target-typecheck-verify-swift -diagnostics-editor-mode -enable-resilience -enable-nonfrozen-enum-exhaustivity-diagnostics

typealias TimeInterval = Double

let NSEC_PER_USEC : UInt64 = 1000
let NSEC_PER_SEC : UInt64 = 1000000000

@_frozen public enum TemporalProxy {
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

public enum NonExhaustive {
  case a, b
}

// Inlineable code is considered "outside" the module and must include a default
// case.
@inlinable
public func testNonExhaustive(_ value: NonExhaustive) {
  switch value { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{do you want to add missing cases?}}
  case .a: break
  }

  switch value { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 {{handle unknown values using "@unknown default"}} {{3-3=@unknown default:\n<#fatalError()#>\n}}
  case .a: break
  case .b: break
  }
  
  switch value {
  case .a: break
  case .b: break
  default: break // no-warning
  }

  switch value {
  case .a: break
  case .b: break
  @unknown case _: break // no-warning
  }
}
