// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -print-diagnostic-groups

// Make sure everything compiles without error when unsafe code is allowed.
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature AllowUnsafeAttribute -warnings-as-errors %s

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

@unsafe
func iAmUnsafe() { }

@unsafe
struct UnsafeType { } // expected-note{{unsafe struct 'UnsafeType' declared here}}

// expected-warning@+1{{reference to unsafe struct 'UnsafeType' [Unsafe]}}
func iAmImpliedUnsafe() -> UnsafeType? { nil }
// expected-note@-1{{make global function 'iAmImpliedUnsafe' @unsafe to indicate that its use is not memory-safe}}{{1-1=@unsafe }}

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
  // expected-note@-1{{make the enclosing struct @unsafe to allow unsafe conformance to protocol 'P'}}{{1-1=@unsafe }}
  @unsafe
  func protoMethod() { } // expected-warning{{unsafe instance method 'protoMethod()' cannot satisfy safe requirement}}
}

@unsafe
struct S2: P {
  @unsafe // okay
  func protoMethod() { }
}

struct S3 { }

extension S3: P {
  // expected-note@-1{{make the enclosing extension @unsafe to allow unsafe conformance to protocol 'P'}}{{1-1=@unsafe }}
  @unsafe
  func protoMethod() { } // expected-warning{{unsafe instance method 'protoMethod()' cannot satisfy safe requirement}}
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
struct UnsafeOuter { // expected-note{{unsafe struct 'UnsafeOuter' declared here}}
  func f(_: UnsafeType) { } // okay

  @unsafe func g(_ y: UnsafeType) {
    let x: UnsafeType = y
    let _ = x
  }
}

// expected-note@+1{{make extension of struct 'UnsafeOuter' @unsafe to indicate that its use is not memory-safe}}{{1-1=@unsafe }}
extension UnsafeOuter { // expected-warning{{reference to unsafe struct 'UnsafeOuter' [Unsafe]}}
  func h(_: UnsafeType) { }
}

@unsafe
extension UnsafeOuter {
  func i(_: UnsafeType) { }
}
