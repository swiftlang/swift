// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: concurrency

func testAsyncSequenceTypedPatternSendable<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int, Seq: Sendable {
   async let result: Int = seq.reduce(0) { $0 + $1 } // OK
   // expected-warning@-1{{immutable value 'result' was never used; consider replacing with '_' or removing it}}
}

func testAsyncSequenceTypedPattern1Sendable<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int, Seq: Sendable {
   async let _: Int = seq.reduce(0) { $0 + $1 } // OK
}

func testAsyncSequenceSendable<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int, Seq: Sendable {
   async let result = seq.reduce(0) { $0 + $1 } // OK
   // expected-warning@-1{{initialization of immutable value 'result' was never used; consider replacing with assignment to '_' or removing it}}
}

func testAsyncSequence1Sendable<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int, Seq: Sendable {
   async let _ = seq.reduce(0) { $0 + $1 } // OK
}

// TODO(diagnostics) [SR-16092]: Add a tailored wording for implicit autoclosure captures in sendable warnings, because 
// from user perspective there is no closure capture, so diagnostic can be confusing.
func testAsyncSequenceTypedPattern<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int {
   async let result: Int = seq.reduce(0) { $0 + $1 } // OK
   // expected-warning@-1{{immutable value 'result' was never used; consider replacing with '_' or removing it}}
   // expected-warning@-2{{capture of 'seq' with non-sendable type 'Seq' in a `@Sendable` closure}}
}

func testAsyncSequenceTypedPattern1<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int {
   async let _: Int = seq.reduce(0) { $0 + $1 } // OK
   // expected-warning@-1{{capture of 'seq' with non-sendable type 'Seq' in a `@Sendable` closure}}
}

func testAsyncSequence<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int {
   async let result = seq.reduce(0) { $0 + $1 } // OK
   // expected-warning@-1{{initialization of immutable value 'result' was never used; consider replacing with assignment to '_' or removing it}}
   // expected-warning@-2{{capture of 'seq' with non-sendable type 'Seq' in a `@Sendable` closure}}
}

func testAsyncSequence1<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int {
   async let _ = seq.reduce(0) { $0 + $1 } // OK
   // expected-warning@-1{{capture of 'seq' with non-sendable type 'Seq' in a `@Sendable` closure}}
}
