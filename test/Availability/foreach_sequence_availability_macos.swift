// RUN: %target-typecheck-verify-swift -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -swift-version 6 -verify-additional-prefix swift6-

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

// ==== --------------------
// An element type with unavailable-Sendable, must be diagnosed as a warning un before-6 mode,
// because previously we've just let this silently slide without error.

struct UnavailableSendableElement {}

// expected-swift5-note@+2 3 {{conformance of 'UnavailableSendableElement' to 'Sendable' has been explicitly marked unavailable here}}
// expected-swift6-note@+1 {{conformance of 'UnavailableSendableElement' to 'Sendable' has been explicitly marked unavailable here}}
@available(*, unavailable)
extension UnavailableSendableElement: @unchecked Sendable {}

struct SendableRequiringSeq<Element: Sendable>: Sequence {
  struct Iterator: IteratorProtocol {
    func next() -> Element? { nil }
  }
  func makeIterator() -> Iterator { Iterator() }
}

// expected-swift5-warning@+2 {{conformance of 'UnavailableSendableElement' to 'Sendable' is unavailable; this is an error in the Swift 6 language mode}}
// expected-swift6-error@+1 {{conformance of 'UnavailableSendableElement' to 'Sendable' is unavailable}}
func testUnavailableSendableRequirement(_ seq: SendableRequiringSeq<UnavailableSendableElement>) {
  for _ in seq {}
  // expected-swift5-warning@-1 {{for-in loop requires 'SendableRequiringSeq<UnavailableSendableElement>' to conform to 'Sequence', which is unavailable; this is an error in the Swift 6 language mode}}
  // expected-swift5-warning@-2 2 {{conformance of 'UnavailableSendableElement' to 'Sendable' is unavailable; this is an error in the Swift 6 language mode}}
  // expected-swift6-error@-3 {{for-in loop requires 'SendableRequiringSeq<UnavailableSendableElement>' to conform to 'Sequence', which is unavailable}}
}

// ==== --------------------
// The 'Sendable' staging above must not leak to a genuinely unavailable 'Sequence' conformance.

struct MixedUnavailableSeq {}

@available(*, unavailable)
extension MixedUnavailableSeq: Sequence, @unchecked Sendable {
  struct Iterator: IteratorProtocol {
    func next() -> Int? { nil }
  }
  func makeIterator() -> Iterator { Iterator() }
}

func testMixedUnavailableConformance(_ seq: MixedUnavailableSeq) {
  for _ in seq {} // expected-error {{for-in loop requires 'MixedUnavailableSeq' to conform to 'Sequence', which is unavailable}}
}
