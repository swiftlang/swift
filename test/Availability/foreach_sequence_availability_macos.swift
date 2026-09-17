// RUN: %target-typecheck-verify-swift

// REQUIRES: OS=macosx

struct SequenceAvailableInFutureMacOS {}

@available(macOS 99, *)
extension SequenceAvailableInFutureMacOS: Sequence {
  struct Iterator: IteratorProtocol {
    func next() -> Int? { nil }
  }
  func makeIterator() -> Iterator {}
}

struct SequenceObsoletedInMacOS10_9 {}

@available(macOS, introduced: 10.8, obsoleted: 10.9)
extension SequenceObsoletedInMacOS10_9: Sequence {
  struct Iterator: IteratorProtocol {
    func next() -> Int? { nil }
  }
  func makeIterator() -> Iterator {}
}

struct SequenceUnavailableInMacOS {}

@available(macOS, unavailable)
extension SequenceUnavailableInMacOS: Sequence {
  struct Iterator: IteratorProtocol {
    func next() -> Int? { nil }
  }
  func makeIterator() -> Iterator {}
}

struct UniversallyUnavailableSequence {}

@available(*, unavailable, message: "use something else")
extension UniversallyUnavailableSequence: Sequence {
  struct Iterator: IteratorProtocol {
    func next() -> Int? { nil }
  }
  func makeIterator() -> Iterator {}
}

func testDeploymentTarget(
  // expected-note@-1 {{add '@available' attribute to enclosing global function}}
  _ futureSeq: SequenceAvailableInFutureMacOS,
  _ obsoleteSeq: SequenceObsoletedInMacOS10_9,
  _ unavailableSeq: SequenceUnavailableInMacOS,
  _ universallyUnavailableSeq: UniversallyUnavailableSequence
) {
  for _ in futureSeq {} // expected-error {{for-in loop requires 'SequenceAvailableInFutureMacOS' to conform to 'Sequence', which is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  for _ in obsoleteSeq {} // expected-error {{for-in loop requires 'SequenceObsoletedInMacOS10_9' to conform to 'Sequence', which is unavailable in macOS}}

  for _ in unavailableSeq {} // expected-error {{for-in loop requires 'SequenceUnavailableInMacOS' to conform to 'Sequence', which is unavailable in macOS}}

  for _ in universallyUnavailableSeq {} // expected-error {{for-in loop requires 'UniversallyUnavailableSequence' to conform to 'Sequence', which is unavailable: use something else}}

  if #available(macOS 99, *) {
    for _ in futureSeq {}
  }
}

@available(macOS 99, *)
func testAvailableInFutureMacOS(
  _ futureSeq: SequenceAvailableInFutureMacOS
) {
  for _ in futureSeq {}
}

@available(macOS, unavailable)
func testUnavailableInMacOS(
  _ unavailableSeq: SequenceUnavailableInMacOS
) {
  for _ in unavailableSeq {}
}
