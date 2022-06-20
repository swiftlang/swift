// RUN: %target-typecheck-verify-swift -disable-availability-checking -strict-concurrency=targeted
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

func testAsyncSequenceTypedPattern<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int { // expected-note{{consider making generic parameter 'Seq' conform to the 'Sendable' protocol}} {{54-54=, Sendable}}
   async let result: Int = seq.reduce(0) { $0 + $1 } // OK
   // expected-warning@-1{{immutable value 'result' was never used; consider replacing with '_' or removing it}}
   // expected-warning@-2{{capture of 'seq' with non-sendable type 'Seq' in 'async let' binding}}
}

func testAsyncSequenceTypedPattern1<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int { // expected-note{{consider making generic parameter 'Seq' conform to the 'Sendable' protocol}} {{55-55=, Sendable}}
   async let _: Int = seq.reduce(0) { $0 + $1 } // OK
   // expected-warning@-1{{capture of 'seq' with non-sendable type 'Seq' in 'async let' binding}}
}

func testAsyncSequence<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int { // expected-note{{consider making generic parameter 'Seq' conform to the 'Sendable' protocol}} {{42-42=, Sendable}}
   async let result = seq.reduce(0) { $0 + $1 } // OK
   // expected-warning@-1{{initialization of immutable value 'result' was never used; consider replacing with assignment to '_' or removing it}}
   // expected-warning@-2{{capture of 'seq' with non-sendable type 'Seq' in 'async let' binding}}
}

func testAsyncSequence1<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int { // expected-note{{consider making generic parameter 'Seq' conform to the 'Sendable' protocol}} {{43-43=, Sendable}}
   async let _ = seq.reduce(0) { $0 + $1 } // OK
   // expected-warning@-1{{capture of 'seq' with non-sendable type 'Seq' in 'async let' binding}}
}

func testAsyncSequence3<Seq>(_ seq: Seq) async throws where Seq: AsyncSequence, Seq.Element == Int { // expected-note{{consider making generic parameter 'Seq' conform to the 'Sendable' protocol}} {{28-28=: Sendable}}
   async let result = seq // expected-warning{{capture of 'seq' with non-sendable type 'Seq' in 'async let' binding}}
   //expected-warning@-1{{initialization of immutable value 'result' was never used; consider replacing with assignment to '_' or removing it}}
}

func testAsyncSequence4<Seq>(_ seq: Seq) async throws where Seq: AsyncSequence, Seq.Element == Int { // expected-note{{consider making generic parameter 'Seq' conform to the 'Sendable' protocol}} {{28-28=: Sendable}}
   async let _ = seq // expected-warning{{capture of 'seq' with non-sendable type 'Seq' in 'async let' binding}}
}
