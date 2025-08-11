// RUN: %target-typecheck-verify-swift -strict-memory-safety

@unsafe
func iAmUnsafe() { }

@unsafe
struct UnsafeType { }

func iAmImpliedUnsafe() -> UnsafeType? { nil }

@unsafe
func labeledUnsafe(_: UnsafeType) {
  unsafe iAmUnsafe()
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  let _ = iAmImpliedUnsafe() // expected-note{{reference to global function 'iAmImpliedUnsafe()' involves unsafe type 'UnsafeType'}}
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  let _ = iAmImpliedUnsafe // expected-note{{reference to global function 'iAmImpliedUnsafe()' involves unsafe type 'UnsafeType'}}
}


class C {
  func method() { } // expected-note{{overridden declaration is here}}
}

class D1: C { // expected-note{{make class 'D1' '@unsafe' to allow unsafe overrides of safe superclass methods}}{{1-1=@unsafe }}
  @unsafe
  override func method() { } // expected-warning{{override of safe instance method with unsafe instance method}}{{documentation-file=strict-memory-safety}}
}

@unsafe class D2: C {
  @unsafe // do not diagnose
  override func method() { }
}

protocol P {
  func protoMethod()
}

struct S1: P {
  // expected-warning@-1{{conformance of 'S1' to protocol 'P' involves unsafe code; use '@unsafe' to indicate that the conformance is not memory-safe}}{{documentation-file=strict-memory-safety}}{{12-12=@unsafe }}
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
  // expected-warning@-1{{conformance of 'S3' to protocol 'P' involves unsafe code; use '@unsafe' to indicate that the conformance is not memory-safe}}{{documentation-file=strict-memory-safety}}{{15-15=@unsafe }}
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
    yield x // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{documentation-file=strict-memory-safety}}
    // expected-note@-1{{reference to unsafe let 'x'}}
  }
  _modify {
    @unsafe var x = 5
    yield &x // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{documentation-file=strict-memory-safety}}
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
  // expected-note@+1{{reference to unsafe instance method 'next()'}}
  for _ in us { } // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{documentation-file=strict-memory-safety}}{{12-12=unsafe }}
  // expected-note@-1{{'@unsafe' conformance of 'UnsafeSequence' to protocol 'Sequence' involves unsafe code}}
  // expected-warning@-2{{for-in loop uses unsafe constructs but is not marked with 'unsafe'}}{{documentation-file=strict-memory-safety}}

  // expected-note@+1{{reference to unsafe instance method 'next()'}}
  for _ in unsafe us { }
  // expected-warning@-1{{for-in loop uses unsafe constructs but is not marked with 'unsafe'}}{{documentation-file=strict-memory-safety}}
}
