// RUN: %target-typecheck-verify-swift -enable-upcoming-feature InferSendableFromCaptures -strict-concurrency=complete -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability
// REQUIRES: swift_feature_InferSendableFromCaptures

class NonSendable : Hashable {
  var data: Int

  init(data: Int = 42) {
    self.data = data
  }

  init(data: [Int]) {
    self.data = data.first!
  }

  static func == (x: NonSendable, y: NonSendable) -> Bool { false }
  func hash(into hasher: inout Hasher) {}
}

final class CondSendable<T> : Hashable {
  init(_: T) {}
  init(_: Int) {}
  init(_: T, other: T = 42) {}
  init<Q>(_: [Q] = []) {}

  static func == (x: CondSendable, y: CondSendable) -> Bool { false }
  func hash(into hasher: inout Hasher) {}
}

extension CondSendable : Sendable where T: Sendable {
}

// Test forming sendable key paths without context
do {
  class K {
    var data: String = ""

    subscript<T>(_: T) -> Bool {
      get { false }
    }

    subscript<Q>(_: Int, _: Q) -> Int {
      get { 42 }
      set {}
    }
  }

  let kp = \K.data // Marked as  `& Sendable`

  let _: KeyPath<K, String> = kp // Ok
  let _: KeyPath<K, String> & Sendable = kp // Ok

  func test<V>(_: KeyPath<K, V> & Sendable) {
  }

  test(kp) // Ok

  let nonSendableKP = \K.[NonSendable()]

  let _: KeyPath<K, Bool> = \.[NonSendable()] // ok
  let _: KeyPath<K, Bool> & Sendable = \.[NonSendable()] // expected-warning {{type 'KeyPath<K, Bool>' does not conform to the 'Sendable' protocol}}
  let _: KeyPath<K, Int> & Sendable = \.[42, NonSendable(data: [-1, 0, 1])] // expected-warning {{type 'ReferenceWritableKeyPath<K, Int>' does not conform to the 'Sendable' protocol}}
  let _: KeyPath<K, Int> & Sendable = \.[42, -1] // Ok

  test(nonSendableKP) // expected-warning {{type 'KeyPath<K, Bool>' does not conform to the 'Sendable' protocol}}
}

// Test using sendable and non-Sendable key paths.
do {
  class V {
    var i: Int = 0

    subscript<T>(_: T) -> Int {
      get { 42 }
    }

    subscript<Q>(_: Int, _: Q) -> Int {
      get { 42 }
      set {}
    }
  }

  func testSendableKP<T, U>(v: T, _ kp: any KeyPath<T, U> & Sendable) {}
  func testSendableFn<T, U>(v: T, _: @Sendable (T) -> U) {}

  func testNonSendableKP<T, U>(v: T, _ kp: KeyPath<T, U>) {}
  func testNonSendableFn<T, U>(v: T, _ kp: (T) -> U) {}

  let v = V()

  testSendableKP(v: v, \.i) // Ok
  testSendableFn(v: v, \.i) // Ok

  testSendableKP(v: v, \.[42]) // Ok
  testSendableFn(v: v, \.[42]) // Ok

  testSendableKP(v: v, \.[NonSendable()]) // expected-warning {{type 'KeyPath<V, Int>' does not conform to the 'Sendable' protocol}}
  testSendableFn(v: v, \.[NonSendable()]) // expected-warning {{converting non-Sendable function value to '@Sendable (V) -> Int' may introduce data races}}

  testNonSendableKP(v: v, \.[NonSendable()]) // Ok
  testNonSendableFn(v: v, \.[NonSendable()]) // Ok

  let _: @Sendable (V) -> Int = \.[NonSendable()]
  // expected-warning@-1 {{converting non-Sendable function value to '@Sendable (V) -> Int' may introduce data races}}

  let _: KeyPath<V, Int> & Sendable = \.[42, CondSendable(NonSendable(data: [1, 2, 3]))]
  // expected-warning@-1 {{type 'ReferenceWritableKeyPath<V, Int>' does not conform to the 'Sendable' protocol}}
  let _: KeyPath<V, Int> & Sendable = \.[42, CondSendable(42)] // Ok

  struct Root {
    let v: V
  }

  testSendableKP(v: v, \.[42, CondSendable(NonSendable(data: [1, 2, 3]))])
  // expected-warning@-1 {{type 'ReferenceWritableKeyPath<V, Int>' does not conform to the 'Sendable' protocol}}
  testSendableFn(v: v, \.[42, CondSendable(NonSendable(data: [1, 2, 3]))])
  // expected-warning@-1 {{converting non-Sendable function value to '@Sendable (V) -> Int' may introduce data races}}
  testSendableKP(v: v, \.[42, CondSendable(42)]) // Ok

  let nonSendable = NonSendable()
  testSendableKP(v: v, \.[42, CondSendable(nonSendable)])
  // expected-warning@-1 {{type 'ReferenceWritableKeyPath<V, Int>' does not conform to the 'Sendable' protocol}}

  testSendableFn(v: v, \.[42, CondSendable(nonSendable)])
  // expected-warning@-1 {{converting non-Sendable function value to '@Sendable (V) -> Int' may introduce data races}}
}

// @dynamicMemberLookup with Sendable requirement
do {
  @dynamicMemberLookup
  struct Test<T> {
    var obj: T

    subscript<U>(dynamicMember member: KeyPath<T, U> & Sendable) -> U {
      get { obj[keyPath: member] }
    }
  }

  _ = Test(obj: "Hello").utf8.count // Ok
}

// Global actor isolated properties.
func testGlobalActorIsolatedReferences() {
  @MainActor struct Isolated {
    var data: Int = 42
    subscript(v: Int) -> Bool { false }
  }

  let dataKP = \Isolated.data // Ok
  let subscriptKP = \Isolated.[42]
  // expected-warning@-1 {{cannot form key path to main actor-isolated subscript 'subscript(_:)'; this is an error in the Swift 6 language mode}}

  let _: KeyPath<Isolated, Int> & Sendable = dataKP
  // expected-warning@-1 {{type 'WritableKeyPath<Isolated, Int>' does not conform to the 'Sendable' protocol}}
  let _: KeyPath<Isolated, Bool> & Sendable = subscriptKP
  // expected-warning@-1 {{type 'KeyPath<Isolated, Bool>' does not conform to the 'Sendable' protocol}}

  func testNonIsolated() {
    _ = \Isolated.data // Ok
  }

  @MainActor func testIsolated() {
    _ = \Isolated.data // Ok
  }
}

@available(SwiftStdlib 5.1, *)
actor SomeActor {
}

@available(SwiftStdlib 5.1, *)
@globalActor
actor GlobalActor {
  static let shared: SomeActor = SomeActor()
}

@available(SwiftStdlib 5.1, *)
func testReferencesToDifferentGlobalActorIsolatedMembers() {
  struct Info {
    @MainActor var name: String  { "" }
  }

  struct Isolated {
    @GlobalActor var info: Info { Info() }
  }

  @MainActor func testIsolatedToMain() {
    _ = \Info.name // Ok
    _ = \Isolated.info.name
    // expected-warning@-1 {{cannot form key path to global actor 'GlobalActor'-isolated property 'info'; this is an error in the Swift 6 language mode}}
  }

  @GlobalActor func testIsolatedToCustom() {
    _ = \Info.name // Ok
    // expected-warning@-1 {{cannot form key path to main actor-isolated property 'name'; this is an error in the Swift 6 language mode}}
    _ = \Isolated.info.name
    // expected-warning@-1 {{cannot form key path to main actor-isolated property 'name'; this is an error in the Swift 6 language mode}}
  }
}

do {
  struct S {
    var a: Int
    var b: String?
  }

  func test<T: Sendable>(_: T) {}

  let kp = [\S.a, \S.b]

  test(kp) // Ok
  test([\S.a, \S.b]) // Ok

  let _: [PartialKeyPath<S>] = [\.a, \.b] // Ok
  let _: [any PartialKeyPath<S> & Sendable] = [\.a, \.b] // Ok
}

do {
  func kp() -> KeyPath<String, Int> & Sendable {
    fatalError()
  }

  // TODO(rdar://125948508): This shouldn't be ambiguous (@Sendable version should be preferred)
  func test() -> KeyPath<String, Int> {
    true ? kp() : kp() // expected-error {{type of expression is ambiguous without a type annotation}}
  }

  func forward<T>(_ v: T) -> T { v }
  // TODO(rdar://125948508): This shouldn't be ambiguous (@Sendable version should be preferred)
  let _: KeyPath<String, Int> = forward(kp()) // expected-error {{conflicting arguments to generic parameter 'T' ('any KeyPath<String, Int> & Sendable' vs. 'KeyPath<String, Int>')}}
}

do {
  final class C<T> {
    let immutable: String = ""
  }

  _ = \C<Int>.immutable as? ReferenceWritableKeyPath // Ok
}

// Should be moved back to sendable_methods.swift once ambiguities are fixed
do {
  struct Test {
    static func fn() {}
    static func otherFn() {}
  }

  // TODO(rdar://125948508): This shouldn't be ambiguous (@Sendable version should be preferred)
  func fnRet(cond: Bool) -> () -> Void {
    cond ? Test.fn : Test.otherFn // expected-error {{type of expression is ambiguous without a type annotation}}
  }

  func forward<T>(_: T) -> T {
  }

  // TODO(rdar://125948508): This shouldn't be ambiguous (@Sendable version should be preferred)
  let _: () -> Void = forward(Test.fn) // expected-error {{conflicting arguments to generic parameter 'T' ('@Sendable () -> ()' vs. '() -> Void')}}
}
