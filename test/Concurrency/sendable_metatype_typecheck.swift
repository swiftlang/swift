// RUN: %target-typecheck-verify-swift -swift-version 6

// REQUIRES: concurrency

// This test checks for typecheck-only diagnostics involving non-Sendable
// metatypes.

protocol Q {
  static func g()
}


// Sendability of existential metatypes
fileprivate nonisolated let anyObjectArray: [AnyClass] = []

func testSendableExistential() {
  _ = anyObjectArray
}


nonisolated func acceptMeta<T>(_: T.Type) { }

nonisolated func staticCallThroughMetaVal<T: Q>(_: T.Type) {
  let x = T.self // expected-warning{{capture of non-Sendable type 'T.Type' in an isolated closure}}
  Task.detached {
    x.g() // expected-warning{{capture of non-Sendable type 'T.Type' in an isolated closure}}
  }
}

nonisolated func captureThroughMetaValMoReqs<T>(_: T.Type) {
  let x = T.self
  Task.detached {
    _ = x
  }
}

nonisolated func passMetaVal<T: Q>(_: T.Type) {
  let x = T.self // expected-warning{{capture of non-Sendable type 'T.Type' in an isolated closure}}
  Task.detached {
    acceptMeta(x) // expected-warning{{capture of non-Sendable type}}
  }
}

nonisolated func staticCallThroughMeta<T: Q>(_: T.Type) {
  Task.detached {
    T.g() // expected-warning{{capture of non-Sendable type}}
  }
}

nonisolated func passMeta<T: Q>(_: T.Type) {
  Task.detached {
    acceptMeta(T.self) // expected-warning{{capture of non-Sendable type 'T.Type' in an isolated closure}}
  }
}


nonisolated func staticCallThroughMetaSendable<T: Sendable & Q>(_: T.Type) {
  Task.detached {
    T.g()
  }
}

nonisolated func passMetaSendable<T: Sendable & Q>(_: T.Type) {
  Task.detached {
    acceptMeta(T.self)
  }
}

nonisolated func passMetaSendableMeta<T: SendableMetatype & Q>(_: T.Type) {
  Task.detached {
    acceptMeta(T.self)
  }
}

nonisolated func passMetaWithSendableVal<T: Sendable & Q>(_: T.Type) {
  let x = T.self
  Task.detached {
    acceptMeta(x) // okay, because T is Sendable implies T.Type: Sendable
    x.g() // okay, because T is Sendable implies T.Type: Sendable
  }
}

nonisolated func passMetaWithMetaSendableVal<T: SendableMetatype & Q>(_: T.Type) {
  let x = T.self
  Task.detached {
    acceptMeta(x) // okay, because T is Sendable implies T.Type: Sendable
    x.g() // okay, because T is Sendable implies T.Type: Sendable
  }
}

struct GenericThingy<Element> {
  func searchMe(_: (Element, Element) -> Bool) { }

  func test() where Element: Comparable {
    // Ensure that this we infer a non-@Sendable function type for Comparable.<
    searchMe(<)

    let _: (Element, Element) -> Bool = (>)
    let _: @Sendable (Element, Element) -> Bool = (>) // expected-error{{converting non-Sendable function value to '@Sendable (Element, Element) -> Bool' may introduce data races}}
  }
}

extension Int: Q {
  static func g() { }
}

extension String: Q {
  static func g() { }
}

class Holder: @unchecked Sendable {
  // expected-note@+3{{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}
  // expected-note@+2{{add '@MainActor' to make static property 'globalExistentialThing' part of global actor 'MainActor'}}
  // expected-warning@+1{{static property 'globalExistentialThing' is not concurrency-safe because non-'Sendable' type 'Dictionary<Int, any Q.Type>' may have shared mutable state}}
  static let globalExistentialThing: Dictionary<Int, Q.Type> = [
    1: Int.self,
    2: String.self,
  ]
}

enum E: Sendable {
case q(Q.Type, Int) // expected-warning{{associated value 'q' of 'Sendable'-conforming enum 'E' contains non-Sendable type 'any Q.Type'}}
}

struct S: Sendable {
  var tuple: ([Q.Type], Int) // expected-warning{{stored property 'tuple' of 'Sendable'-conforming struct 'S' contains non-Sendable type 'any Q.Type'}}
}

extension Q {
  static func h() -> Self { }
}

extension Array: Q where Element: Q {
  static func g() { }
}

struct GenericS<T> { }

extension GenericS: Q where T: Q {
  static func g() { }
}

extension GenericS: Sendable where T: Sendable { }

final class TestStaticMembers<T> {
  init(_: T) {
    let _: @Sendable () -> GenericS<Int> = GenericS.h // Ok
  }
}

// Downgrade use of static member references on non-Sendable base to a warning until next major mode to maintain source compatibility.
do {
  struct S<U> {
    static func test(_: U, _: U) -> Bool { false }
  }

  func compute<T>(_: S<T>, _: @escaping @Sendable (T, T) -> Bool) {}

  func test<T: Comparable>(s: S<T>) {
    compute(s, >) // expected-warning {{converting non-Sendable function value to '@Sendable (T, T) -> Bool' may introduce data races}}
    compute(s, S.test) // expected-warning {{capture of non-Sendable type 'T.Type' in an isolated closure}}
  }
}

infix operator <=> : ComparisonPrecedence

struct TestUnapplied<U> : Comparable {
  static func <(_: TestUnapplied<U>, _: TestUnapplied<U>) -> Bool { false }
}

extension TestUnapplied {
  static func <=>(_: Self, _: @escaping @Sendable (U, U) -> Bool) {}
}

func testUnappliedWithOpetator<T: Comparable>(v: TestUnapplied<T>) {
  v<=>(>) // expected-error {{converting non-Sendable function value to '@Sendable (T, T) -> Bool' may introduce data races}}
}

protocol P {}

func acceptClosure(_: () -> Void) {}

@MainActor
func f<T: P>(_: T.Type) {
  acceptClosure {
    Task {
      _ = T.self // okay to capture T.Type in this closure.
    }
  }
}
