// RUN: %target-swift-frontend -typecheck -verify -strict-concurrency=complete %s

// Don't test TransferNonSendable because this test will not make
// it past Sema to the SIL pass.

// REQUIRES: concurrency

// rdar://82452688 - make sure sendable checking doesn't fire for a capture
// of a value of error-type
@available(SwiftStdlib 5.1, *)
func f() async {
  let n = wobble() // expected-error{{cannot find 'wobble' in scope}}
  @Sendable func nested() {
    n.pointee += 1
  }
}

struct S: Sendable {
  var b: Undefined // expected-error{{cannot find type 'Undefined' in scope}}
}
