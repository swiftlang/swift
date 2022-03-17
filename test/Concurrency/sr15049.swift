// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: concurrency

func testAsyncSequenceTypedPattern<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int {
   async let result: Int = seq.reduce(0) { $0 + $1 } // OK
   // expected-warning@-1{{immutable value 'result' was never used; consider replacing with '_' or removing it}}
}

func testAsyncSequenceTypedPattern1<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int {
   async let _: Int = seq.reduce(0) { $0 + $1 } // OK
}

func testAsyncSequence<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int {
   async let result = seq.reduce(0) { $0 + $1 } // OK
   // expected-warning@-1{{initialization of immutable value 'result' was never used; consider replacing with assignment to '_' or removing it}}
}

func testAsyncSequence1<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int {
   async let _ = seq.reduce(0) { $0 + $1 } // OK
}
