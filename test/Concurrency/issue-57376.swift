// RUN: %target-swift-frontend -disable-availability-checking -strict-concurrency=targeted %s -emit-sil -o /dev/null -verify -verify-additional-prefix targeted-
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -verify-additional-prefix tns- -verify-additional-prefix transferring-tns-

// REQUIRES: concurrency
// REQUIRES: asserts

// https://github.com/apple/swift/issues/57376

func testAsyncSequenceTypedPatternSendable<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int, Seq: Sendable {
  async let result: Int = seq.reduce(0) { $0 + $1 } // OK
  let _ = try! await result
}

func testAsyncSequenceTypedPattern1Sendable<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int, Seq: Sendable {
  async let _: Int = seq.reduce(0) { $0 + $1 } // OK
}

func testAsyncSequenceSendable<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int, Seq: Sendable {
  async let result = seq.reduce(0) { $0 + $1 } // OK
  let _ = try! await result
}

func testAsyncSequence1Sendable<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int, Seq: Sendable {
  async let _ = seq.reduce(0) { $0 + $1 } // OK
}

func testAsyncSequenceTypedPattern<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int {
  async let result: Int = seq.reduce(0) { $0 + $1 } // expected-transferring-tns-warning {{sending 'seq' risks causing data races}}
  // expected-transferring-tns-note @-1 {{sending task-isolated 'seq' into async let risks causing data races between nonisolated and task-isolated uses}}
  let _ = try! await result
}

func testAsyncSequenceTypedPattern1<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int {
  async let _: Int = seq.reduce(0) { $0 + $1 } // expected-transferring-tns-warning {{sending 'seq' risks causing data races}}
  // expected-transferring-tns-note @-1 {{sending task-isolated 'seq' into async let risks causing data races between nonisolated and task-isolated uses}}
}

func testAsyncSequence<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int {
  async let result = seq.reduce(0) { $0 + $1 } // expected-transferring-tns-warning {{sending 'seq' risks causing data races}}
  // expected-transferring-tns-note @-1 {{sending task-isolated 'seq' into async let risks causing data races between nonisolated and task-isolated uses}}
  let _ = try! await result
}

func testAsyncSequence1<Seq: AsyncSequence>(_ seq: Seq) async throws where Seq.Element == Int {
  async let _ = seq.reduce(0) { $0 + $1 } // expected-transferring-tns-warning {{sending 'seq' risks causing data races}}
  // expected-transferring-tns-note @-1 {{sending task-isolated 'seq' into async let risks causing data races between nonisolated and task-isolated uses}}
}

func testAsyncSequence3<Seq>(_ seq: Seq) async throws where Seq: AsyncSequence, Seq.Element == Int {
  async let result = seq // expected-transferring-tns-warning {{sending 'seq' risks causing data races}}
  // expected-transferring-tns-note @-1 {{sending task-isolated 'seq' into async let risks causing data races between nonisolated and task-isolated uses}}
  let _ = await result
}

func testAsyncSequence4<Seq>(_ seq: Seq) async throws where Seq: AsyncSequence, Seq.Element == Int {
  async let _ = seq // expected-transferring-tns-warning {{sending 'seq' risks causing data races}}
  // expected-transferring-tns-note @-1 {{sending task-isolated 'seq' into async let risks causing data races between nonisolated and task-isolated uses}}
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
