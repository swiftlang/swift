// RUN: %target-swift-frontend -require-explicit-sendable -strict-concurrency=complete %s -emit-sil -o /dev/null -verify


public protocol P { }

// expected-note@+2{{consider making struct 'S1' conform to the 'Sendable' protocol}}{{18-18=: Sendable}}
// expected-note@+1{{make struct 'S1' explicitly non-Sendable to suppress this warning}}{{+2:2-2=\n\n@available(*, unavailable)\nextension S1: Sendable { \}\n}}
public struct S1 { // expected-warning{{public struct 'S1' does not specify whether it is 'Sendable' or not}}
  var str: String
}

class C { }

// expected-note@+1{{make struct 'S2' explicitly non-Sendable to suppress this warning}}{{+2:2-2=\n\n@available(*, unavailable)\nextension S2: Sendable { \}\n}}
public struct S2 { // expected-warning{{public struct 'S2' does not specify whether it is 'Sendable' or not}}
  var c: C
}

// expected-note@+2{{consider making class 'C1' conform to the 'Sendable' protocol}}{{25-25=, Sendable}}
// expected-note@+1{{make class 'C1' explicitly non-Sendable to suppress this warning}}{{+2:2-2=\n\n@available(*, unavailable)\nextension C1: Sendable { \}\n}}
final public class C1: P { // expected-warning{{public class 'C1' does not specify whether it is 'Sendable' or not}}
  let str: String = ""
}

// expected-note@+1{{make class 'C2' explicitly non-Sendable to suppress this warning}}{{+2:2-2=\n\n@available(*, unavailable)\nextension C2: Sendable { \}\n}}
public class C2 { // expected-warning{{public class 'C2' does not specify whether it is 'Sendable' or not}}
  var str: String = ""
}

// expected-note@+2{{consider making generic struct 'S3' conform to the 'Sendable' protocol}}{{+2:2-2=\n\nextension S3: Sendable where T: Sendable { \}\n}}
// expected-note@+1{{make generic struct 'S3' explicitly non-Sendable to suppress this warning}}{{+2:2-2=\n\n@available(*, unavailable)\nextension S3: Sendable { \}\n}}
public struct S3<T> { // expected-warning{{public generic struct 'S3' does not specify whether it is 'Sendable' or not}}
  var t: T
}

// expected-note@+1{{make generic struct 'S4' explicitly non-Sendable to suppress this warning}}{{+3:2-2=\n\n@available(*, unavailable)\nextension S4: Sendable { \}\n}}
public struct S4<T> { // expected-warning{{public generic struct 'S4' does not specify whether it is 'Sendable' or not}}
  var t: T
  var c: C
}

public struct S5 { } // no diagnostic: S5 is not Sendable

@available(*, unavailable)
extension S5: Sendable { }
// expected-note@-1{{conformance of 'S5' to 'Sendable' has been explicitly marked unavailable here}}

// Public type with a conditional conformance, so don't complain
public struct S6<T, U> {
  var t: T
  var u: U
}

extension S6: Sendable where T: Sendable, U: Sendable { }

func acceptSendable<T: Sendable>(_: T) {
}

struct S7 { // FIXME: expected-note{{consider making struct 'S7' conform to the 'Sendable' protocol}}
}

@available(*, unavailable)
extension S7: Sendable { }
// expected-note@-1{{conformance of 'S7' to 'Sendable' has been explicitly marked unavailable here}}


func testMe(s5: S5, s7: S7) {
  acceptSendable(s5) // expected-warning{{conformance of 'S5' to 'Sendable' is unavailable}}
  acceptSendable(s7) // expected-warning{{conformance of 'S7' to 'Sendable' is unavailable}}
}

// expected-note@+2{{consider making generic struct 'S8' conform to the 'Sendable' protocol}}{{+2:2-2=\n\nextension S8: Sendable where T: Sendable, U: Sendable, V: Sendable { \}\n}}
// expected-note@+1{{make generic struct 'S8' explicitly non-Sendable to suppress this warning}}
public struct S8<T: Hashable, U, V> { // expected-warning{{public generic struct 'S8' does not specify whether it is 'Sendable' or not}}
  var member: [T: (U, V?)]
}

public protocol P2 {
  associatedtype A
}

// expected-warning@+3{{public generic struct 'S9' does not specify whether it is 'Sendable' or not}}
// expected-note@+2{{consider making generic struct 'S9' conform to the 'Sendable' protocol}}{{+2:2-2=\n\nextension S9: Sendable where T: Sendable, T.A: Sendable { \}\n}}
// expected-note@+1{{make generic struct 'S9' explicitly non-Sendable to suppress this warning}}
public struct S9<T: P2 & Hashable> {
  var dict: [T : T.A] = [:]
}

public struct S10 { // expected-warning{{public struct 'S10' does not specify whether it is 'Sendable' or not}}
  // expected-note@-1{{make struct 'S10' explicitly non-Sendable to suppress this warning}}
  var s7: S7
}

struct S11: Sendable {
  var s7: S7 // expected-warning{{stored property 's7' of 'Sendable'-conforming struct 'S11' has non-Sendable type 'S7'}}
}

@_nonSendable public struct S12 { }

// Don't complain about global-actor-qualified classes or their subclasses.
@available(SwiftStdlib 5.1, *)
@MainActor
open class TestThing {}

@available(SwiftStdlib 5.1, *)
open class TestSubThing : TestThing {}

@available(SwiftStdlib 5.1, *)
@MainActor(unsafe) // expected-warning {{'(unsafe)' global actors are deprecated; use '@preconcurrency' instead}}
open class TestThing2 {}

@available(SwiftStdlib 5.1, *)
open class TestSubThing2 : TestThing2 {}
