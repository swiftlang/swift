// RUN: %target-typecheck-verify-swift -require-explicit-sendable

public protocol P { }

public struct S1 { // expected-warning{{public struct 'S1' does not specify whether it is 'Sendable' or not}}
  // expected-note@-1{{consider making struct 'S1' conform to the 'Sendable' protocol}}{{18-18=: Sendable}}
  // expected-note@-2{{make struct 'S1' explicitly non-Sendable to suppress this warning}}{{2-2=\n\n@available(*, unavailable)\nextension S1: Sendable { \}\n}}
  var str: String
}

class C { }

public struct S2 { // expected-warning{{public struct 'S2' does not specify whether it is 'Sendable' or not}}
  // expected-note@-1{{add '@unchecked Sendable' conformance to struct 'S2' if this type manually implements concurrency safety}}{{18-18=: @unchecked Sendable}}
  // expected-note@-2{{make struct 'S2' explicitly non-Sendable to suppress this warning}}{{2-2=\n\n@available(*, unavailable)\nextension S2: Sendable { \}\n}}
  var c: C
}

final public class C1: P { // expected-warning{{public class 'C1' does not specify whether it is 'Sendable' or not}}
  // expected-note@-1{{consider making class 'C1' conform to the 'Sendable' protocol}}{{25-25=, Sendable}}
  // expected-note@-2{{make class 'C1' explicitly non-Sendable to suppress this warning}}{{2-2=\n\n@available(*, unavailable)\nextension C1: Sendable { \}\n}}
  let str: String = ""
}

public class C2 { // expected-warning{{public class 'C2' does not specify whether it is 'Sendable' or not}}
  // expected-note@-1{{add '@unchecked Sendable' conformance to class 'C2' if this type manually implements concurrency safety}}{{17-17=: @unchecked Sendable}}
  // expected-note@-2{{make class 'C2' explicitly non-Sendable to suppress this warning}}{{2-2=\n\n@available(*, unavailable)\nextension C2: Sendable { \}\n}}
  var str: String = ""
}

public struct S3<T> { // expected-warning{{public generic struct 'S3' does not specify whether it is 'Sendable' or not}}
  // expected-note@-1{{add '@unchecked Sendable' conformance to generic struct 'S3' if this type manually implements concurrency safety}}{{21-21=: @unchecked Sendable}}
  // expected-note@-2{{make generic struct 'S3' explicitly non-Sendable to suppress this warning}}{{2-2=\n\n@available(*, unavailable)\nextension S3: Sendable { \}\n}}
  var t: T
}

public struct S4<T> { // expected-warning{{public generic struct 'S4' does not specify whether it is 'Sendable' or not}}
  // expected-note@-1{{add '@unchecked Sendable' conformance to generic struct 'S4' if this type manually implements concurrency safety}}{{21-21=: @unchecked Sendable}}
  // expected-note@-2{{make generic struct 'S4' explicitly non-Sendable to suppress this warning}}{{2-2=\n\n@available(*, unavailable)\nextension S4: Sendable { \}\n}}
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

struct S7 {
}

@available(*, unavailable)
extension S7: Sendable { }
// expected-note@-1{{conformance of 'S7' to 'Sendable' has been explicitly marked unavailable here}}


func testMe(s5: S5, s7: S7) {
  acceptSendable(s5) // expected-warning{{conformance of 'S5' to 'Sendable' is unavailable}}
  acceptSendable(s7) // expected-warning{{conformance of 'S7' to 'Sendable' is unavailable}}
}
