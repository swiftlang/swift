// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -print-diagnostic-groups

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

@unsafe
func iAmUnsafe() { }

@unsafe
struct UnsafeType { }

// expected-note@+3{{reference to unsafe struct 'UnsafeType'}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{1-1=@safe }}
func iAmImpliedUnsafe() -> UnsafeType? { nil }
// expected-warning@-1{{global function 'iAmImpliedUnsafe' has an interface that involves unsafe types}}

@unsafe
func labeledUnsafe(_: UnsafeType) {
  unsafe iAmUnsafe()
  let _ = unsafe iAmImpliedUnsafe()
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

extension S4: @unsafe P {
  @unsafe
  func protoMethod() { } // okay
}

protocol P2 {
  func proto2Method()
}

@unsafe
extension S4: P2 { // expected-warning{{conformance of 'S4' to protocol 'P2' involves unsafe code; use '@unsafe' to indicate that the conformance is not memory-safe}}
  @unsafe
  func proto2Method() { } // expected-note{{unsafe instance method}}
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
    @unsafe let x: UnsafeType = unsafe y
    let _ = unsafe x
  }
}

extension UnsafeOuter {
  func h(_: UnsafeType) { }
}

@unsafe
extension UnsafeOuter {
  func i(_: UnsafeType) { }
}

// -----------------------------------------------------------------------
// Miscellaneous issues
// -----------------------------------------------------------------------
var yieldUnsafe: Int {
  _read {
    @unsafe let x = 5
    yield x // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe' [Unsafe]}}
    // expected-note@-1{{reference to unsafe let 'x'}}
  }
  _modify {
    @unsafe var x = 5
    yield &x // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe' [Unsafe]}}
    // expected-note@-1{{reference to unsafe var 'x'}}
  }
}

var yieldUnsafeOkay: Int {
  _read {
    @unsafe let x = 5
    yield unsafe x
  }
  _modify {
    @unsafe var x = 5
    yield unsafe &x
  }
}

struct UnsafeSequence: @unsafe IteratorProtocol, @unsafe Sequence {
  @unsafe func next() -> Int? { nil }
}

func forEachLoop(us: UnsafeSequence) {
  for _ in us { } // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe' [Unsafe]}}{{12-12=unsafe }}
  // expected-note@-1{{@unsafe conformance of 'UnsafeSequence' to protocol 'Sequence' involves unsafe code}}
  // expected-note@-2{{reference to unsafe instance method 'next()'}}

  for _ in unsafe us { }
}
