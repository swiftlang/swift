// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -print-diagnostic-groups

// Make sure everything compiles without error when unsafe code is allowed.
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature AllowUnsafeAttribute -warnings-as-errors %s

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

@unsafe
func iAmUnsafe() { }

@unsafe
struct UnsafeType { }

// expected-note@+1{{reference to unsafe struct 'UnsafeType'}}
func iAmImpliedUnsafe() -> UnsafeType? { nil }
// expected-warning@-1{{global function 'iAmImpliedUnsafe' involves unsafe code; use '@unsafe' to indicate that its use is not memory-safe [Unsafe]}}{{1-1=@unsafe }}

@unsafe
func labeledUnsafe(_: UnsafeType) {
  iAmUnsafe()
  let _ = iAmImpliedUnsafe()
}


class C {
  func method() { } // expected-note{{overridden declaration is here}}
}

class D1: C { // expected-note{{make class 'D1' @unsafe to allow unsafe overrides of safe superclass methods}}{{1-1=@unsafe }}
  @unsafe
  override func method() { } // expected-warning{{override of safe instance method with unsafe instance method [Unsafe]}}
}

@unsafe class D2: C {
  @unsafe // do not diagnose
  override func method() { }
}

protocol P {
  func protoMethod()
}

struct S1: P {
  // expected-warning@-1{{conformance of 'S1' to protocol 'P' involves unsafe code; use '@unsafe' to indicate that the conformance is not memory-safe [Unsafe]}}{{12-12=@unsafe }}
  @unsafe
  func protoMethod() { } // expected-note{{unsafe instance method 'protoMethod()' cannot satisfy safe requirement}}
}

@unsafe
struct S2: P {
  @unsafe
  func protoMethod() { }
}

struct S3 { }

extension S3: P {
  // expected-warning@-1{{conformance of 'S3' to protocol 'P' involves unsafe code; use '@unsafe' to indicate that the conformance is not memory-safe [Unsafe]}}{{15-15=@unsafe }}
  @unsafe
  func protoMethod() { } // expected-note{{unsafe instance method 'protoMethod()' cannot satisfy safe requirement}}
}

struct S4 { }

@unsafe
extension S4: P {
  @unsafe
  func protoMethod() { } // okay
}


@unsafe
class SendableC1: @unchecked Sendable { }

class SendableC2 { }

@unsafe
extension SendableC2: @unchecked Sendable { }


struct Wrapper {
  @unsafe
  unowned(unsafe) var reference: C
}

@_nonSendable
class NonSendable { }

@unsafe nonisolated(unsafe) var notSendable: NonSendable = .init() // okay


@unsafe
struct UnsafeOuter {
  func f(_: UnsafeType) { } // okay

  @unsafe func g(_ y: UnsafeType) {
    let x: UnsafeType = y
    let _ = x
  }
}

// expected-warning@+1{{extension of struct 'UnsafeOuter' involves unsafe code; use '@unsafe' to indicate that its use is not memory-safe}}{{1-1=@unsafe }}
extension UnsafeOuter { // expected-note{{reference to unsafe struct 'UnsafeOuter'}}
  func h(_: UnsafeType) { }
}

@unsafe
extension UnsafeOuter {
  func i(_: UnsafeType) { }
}
