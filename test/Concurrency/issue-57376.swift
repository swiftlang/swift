// RUN: %target-swift-frontend -disable-availability-checking -strict-concurrency=targeted %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-experimental-feature SendNonSendable

// REQUIRES: concurrency
// REQUIRES: asserts

// https://github.com/apple/swift/issues/57376

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

func search(query: String, entities: [String]) async throws -> [String] {
  async let r = entities.filter { $0.contains(query) }.map { String($0) }
  return await r // OK
}

@rethrows protocol TestRethrowProtocol {
    func fn() async throws
}
extension TestRethrowProtocol {
    func testRethrow() async rethrows {
        try await self.fn()
    }
}

struct TestRethrowStruct: TestRethrowProtocol {
    func fn() async throws {}
}

func testStructRethrows() async throws {
   let s = TestRethrowStruct()
   async let rt: () = s.testRethrow()
   try await rt // OK
}

// https://github.com/apple/swift/issues/60351
func foo() async {
    let stream = AsyncStream<Int>{ _ in }
    async let bar = stream.first { _ in true}

    _ = await bar // OK
}
