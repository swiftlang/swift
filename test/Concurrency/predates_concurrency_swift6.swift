// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-version 6 %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency

@preconcurrency func unsafelySendableClosure(_ closure: @Sendable () -> Void) { }

@preconcurrency func unsafelyMainActorClosure(_ closure: @MainActor () -> Void) { }

@preconcurrency func unsafelyDoEverythingClosure(_ closure: @MainActor @Sendable () -> Void) { }

struct X {
  @preconcurrency func unsafelyDoEverythingClosure(_ closure: @MainActor @Sendable () -> Void) { }

  @preconcurrency var sendableVar: @Sendable () -> Void
  @preconcurrency var mainActorVar: @MainActor () -> Void

  @preconcurrency
  subscript(_: @MainActor () -> Void) -> (@Sendable () -> Void) { {} }

  @preconcurrency
  static subscript(statically _: @MainActor () -> Void) -> (@Sendable () -> Void) { { } }
}

@MainActor func onMainActor() { }

func testInAsync(x: X) async {
  let _: Int = unsafelySendableClosure // expected-error{{type '@Sendable (@Sendable () -> Void) -> ()'}}
  let _: Int = unsafelyMainActorClosure // expected-error{{type '@Sendable (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = unsafelyDoEverythingClosure // expected-error{{type '@Sendable (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = x.unsafelyDoEverythingClosure // expected-error{{type '@Sendable (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = X.unsafelyDoEverythingClosure // expected-error{{type '@Sendable (X) -> @Sendable (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = (X.unsafelyDoEverythingClosure)(x) // expected-error{{type '@Sendable (@MainActor @Sendable () -> Void) -> ()'}}

  let _: Int = x.sendableVar // expected-error{{type '@Sendable () -> Void'}}
  let _: Int = x.mainActorVar // expected-error{{type '@MainActor @Sendable () -> Void'}}

  let _: Int = x[{ onMainActor() }] // expected-error{{type '@Sendable () -> Void'}}
  let _: Int = X[statically: { onMainActor() }] // expected-error{{type '@Sendable () -> Void'}}
}

func testElsewhere(x: X) {
  let _: Int = unsafelySendableClosure // expected-error{{type '@Sendable (@Sendable () -> Void) -> ()'}}
  let _: Int = unsafelyMainActorClosure // expected-error{{type '@Sendable (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = unsafelyDoEverythingClosure // expected-error{{type '@Sendable (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = x.unsafelyDoEverythingClosure // expected-error{{type '@Sendable (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = X.unsafelyDoEverythingClosure // expected-error{{type '@Sendable (X) -> @Sendable (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = (X.unsafelyDoEverythingClosure)(x) // expected-error{{type '@Sendable (@MainActor @Sendable () -> Void) -> ()'}}

  let _: Int = x.sendableVar // expected-error{{type '@Sendable () -> Void'}}
  let _: Int = x.mainActorVar // expected-error{{type '@MainActor @Sendable () -> Void'}}

  let _: Int = x[{ onMainActor() }] // expected-error{{type '@Sendable () -> Void'}}
  let _: Int = X[statically: { onMainActor() }] // expected-error{{type '@Sendable () -> Void'}}
}

@MainActor @preconcurrency func onMainActorAlways() { }
// expected-note@-1{{are implicitly asynchronous}}

@preconcurrency @MainActor class MyModelClass {
 func f() { }
  // expected-note@-1{{are implicitly asynchronous}}
}

func testCalls(x: X) {
  // expected-note@-1 2{{add '@MainActor' to make global function 'testCalls(x:)' part of global actor 'MainActor'}}
  onMainActorAlways() // expected-error{{call to main actor-isolated global function 'onMainActorAlways()' in a synchronous nonisolated context}}

  let _: () -> Void = onMainActorAlways // expected-error{{converting function value of type '@MainActor @Sendable () -> ()' to '() -> Void' loses global actor 'MainActor'}}

  let c = MyModelClass() // okay, synthesized init() is 'nonisolated'

  c.f() // expected-error{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
}

func testCallsWithAsync() async {
  onMainActorAlways() // expected-error{{main actor-isolated global function 'onMainActorAlways()' cannot be called from outside of the actor}} {{3-3=await }}

  let _: () -> Void = onMainActorAlways // expected-error{{converting function value of type '@MainActor @Sendable () -> ()' to '() -> Void' loses global actor 'MainActor'}}

  let c = MyModelClass() // okay, synthesized init() is 'nonisolated'

  c.f() // expected-error{{main actor-isolated instance method 'f()' cannot be called from outside of the actor}} {{3-3=await }}
}

// ---------------------------------------------------------------------------
// Protocols that inherit Sendable and predate concurrency.
// ---------------------------------------------------------------------------
@preconcurrency protocol P: Sendable { }
protocol Q: P { }

class NS { } // expected-note 5{{class 'NS' does not conform to the 'Sendable' protocol}}

struct S1: P {
  var ns: NS // expected-warning{{stored property 'ns' of 'Sendable'-conforming struct 'S1' has non-Sendable type 'NS'}}
}

struct S2: Q {
  var ns: NS // expected-warning{{stored property 'ns' of 'Sendable'-conforming struct 'S2' has non-Sendable type 'NS'}}
}

struct S3: Q, Sendable {
  var ns: NS // expected-error{{stored property 'ns' of 'Sendable'-conforming struct 'S3' has non-Sendable type 'NS'}}
}

// ---------------------------------------------------------------------------
// Historical attribute names do nothing (but are permitted)
// ---------------------------------------------------------------------------
func aFailedExperiment(@_unsafeSendable _ body: @escaping () -> Void) { }
// expected-warning@-1{{'_unsafeSendable' attribute has been removed in favor of '@preconcurrency'}}

func anothingFailedExperiment(@_unsafeMainActor _ body: @escaping () -> Void) { }
// expected-warning@-1{{'_unsafeMainActor' attribute has been removed in favor of '@preconcurrency'}}

// Override matching with @preconcurrency properties.
do {
  class Base {
    @preconcurrency
    open var test1 : ([any Sendable])? // expected-note {{overridden declaration is here}}

    @preconcurrency
    open var test2: [String: [Int: any Sendable]] // expected-note {{overridden declaration is here}}

    @preconcurrency
    open var test3: any Sendable // expected-note {{overridden declaration is here}}

    @preconcurrency
    open var test4: (((Any)?) -> Void)? { // expected-note {{overridden declaration is here}}
      nil
    }

    @preconcurrency
    open var test5: (@MainActor () -> Void)? { // expected-note {{overridden declaration is here}}
      nil
    }

    @preconcurrency
    func test6(_: (@MainActor () -> Void)? = nil) { // expected-note {{overridden declaration is here}}
    }

    init() {
      self.test1 = nil
      self.test2 = [:]
      self.test3 = 42
    }
  }

  class Test : Base {
    override var test1: [Any]? {
      // expected-error@-1 {{declaration 'test1' has a type with different sendability from any potential overrides}}
      get { nil }
      set { }
    }

    override var test2: [String: [Int: Any]] {
      // expected-error@-1 {{declaration 'test2' has a type with different sendability from any potential overrides}}
      get { [:] }
      set {}
    }

    override var test3: Any {
      // expected-error@-1 {{declaration 'test3' has a type with different sendability from any potential overrides}}
      get { 42 }
      set { }
    }

    override var test4: (((any Sendable)?) -> Void)? {
      // expected-error@-1 {{declaration 'test4' has a type with different sendability from any potential overrides}}
      nil
    }

    override var test5: (() -> Void)? {
      // expected-error@-1 {{declaration 'test5' has a type with different global actor isolation from any potential overrides}}
      nil
    }

    override func test6(_: (() -> Void)?) {
      // expected-error@-1 {{declaration 'test6' has a type with different global actor isolation from any potential overrides}}
    }
  }
}



@preconcurrency
func withSendableClosure(_: @Sendable () -> Void) {}

func conversionDowngrade() {
  let ns: () -> Void = {}
  withSendableClosure(ns)
  // expected-warning@-1 {{converting non-Sendable function value to '@Sendable () -> Void' may introduce data races}}
}

@preconcurrency
func requireSendable<T: Sendable>(_: T) {}

@preconcurrency
struct RequireSendable<T: Sendable> {}

class NotSendable {} // expected-note 8 {{class 'NotSendable' does not conform to the 'Sendable' protocol}}

class UnavailableSendable {}

@available(*, unavailable)
extension UnavailableSendable: @unchecked Sendable {}
// expected-note@-1 8 {{conformance of 'UnavailableSendable' to 'Sendable' has been explicitly marked unavailable here}}

typealias T = RequireSendable<NotSendable>
// expected-warning@-1 {{type 'NotSendable' does not conform to the 'Sendable' protocol}}

typealias T2 = RequireSendable<UnavailableSendable>
// expected-warning@-1 {{conformance of 'UnavailableSendable' to 'Sendable' is unavailable}}

class C {
  @preconcurrency
  func requireSendable<T: Sendable>(_: T) {}

  @preconcurrency
  static func requireSendableStatic<T: Sendable>(_: T) {}
}

func testRequirementDowngrade(ns: NotSendable, us: UnavailableSendable, c: C) {
  requireSendable(ns)
  // expected-warning@-1 {{type 'NotSendable' does not conform to the 'Sendable' protocol}}

  c.requireSendable(ns)
  // expected-warning@-1 {{type 'NotSendable' does not conform to the 'Sendable' protocol}}

  C.requireSendableStatic(ns)
  // expected-warning@-1 {{type 'NotSendable' does not conform to the 'Sendable' protocol}}

  requireSendable(us)
  // expected-warning@-1 {{conformance of 'UnavailableSendable' to 'Sendable' is unavailable}}

  c.requireSendable(us)
  // expected-warning@-1 {{conformance of 'UnavailableSendable' to 'Sendable' is unavailable}}

  C.requireSendableStatic(us)
  // expected-warning@-1 {{conformance of 'UnavailableSendable' to 'Sendable' is unavailable}}
}


protocol P2 {}

extension NotSendable: P2 {}

extension UnavailableSendable: P2 {}

@preconcurrency
func requireSendableExistential(_: any P2 & Sendable) {}

func requireSendableExistentialAlways(_: any P2 & Sendable) {}

extension C {
  @preconcurrency
  func requireSendableExistential(_: any P2 & Sendable) {}

  @preconcurrency
  static func requireSendableExistentialStatic(_: any P2 & Sendable) {}
}

func testErasureDowngrade(ns: NotSendable, us: UnavailableSendable, c: C) {
  requireSendableExistential(ns)
  // expected-warning@-1 {{type 'NotSendable' does not conform to the 'Sendable' protocol}}

  c.requireSendableExistential(ns)
  // expected-warning@-1 {{type 'NotSendable' does not conform to the 'Sendable' protocol}}

  C.requireSendableExistentialStatic(ns)
  // expected-warning@-1 {{type 'NotSendable' does not conform to the 'Sendable' protocol}}

  requireSendableExistential(us)
  // expected-warning@-1 {{conformance of 'UnavailableSendable' to 'Sendable' is unavailable}}

  c.requireSendableExistential(us)
  // expected-warning@-1 {{conformance of 'UnavailableSendable' to 'Sendable' is unavailable}}

  C.requireSendableExistentialStatic(us)
  // expected-warning@-1 {{conformance of 'UnavailableSendable' to 'Sendable' is unavailable}}

  withSendableClosure {
    let ns = NotSendable()
    requireSendableExistentialAlways(ns)
    // expected-error@-1 {{type 'NotSendable' does not conform to the 'Sendable' protocol}}

    let us = UnavailableSendable()
    requireSendableExistentialAlways(us)
    // expected-error@-1 {{conformance of 'UnavailableSendable' to 'Sendable' is unavailable}}
  }
}

// The member itself could be non-preconcurrency but the base could be.
do {
  @preconcurrency var d: [String: any Sendable] = [:]

  let data: [String: Any] = [:]
  d.merge(data, uniquingKeysWith: { _, rhs in rhs})
  // expected-warning@-1 {{type 'Any' does not conform to the 'Sendable' protocol}}

  struct Test {
    @preconcurrency var info: [String: any Sendable] = [:]
  }

  func test(s: inout Test) {
    s.info["hello"] = { }
    // expected-warning@-1 {{type '() -> ()' does not conform to the 'Sendable' protocol}}
    // expected-note@-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  }

  // If destination is @preconcurrency the Sendable conformance error should be downgraded
  d = data // expected-warning {{type 'Any' does not conform to the 'Sendable' protocol}}
}

do {
  final class Mutating: P {
    var state: Int = 0 // expected-warning {{stored property 'state' of 'Sendable'-conforming class 'Mutating' is mutable}}
  }

  struct StructWithInit: P {
    let prop = NS() // expected-warning {{stored property 'prop' of 'Sendable'-conforming struct 'StructWithInit' has non-Sendable type 'NS'}}
  }

  enum E : P {
    case test(NS) // expected-warning {{associated value 'test' of 'Sendable'-conforming enum 'E' has non-Sendable type 'NS'}}
  }
}

func testSendableMetatypeDowngrades() {
  @preconcurrency
  func acceptsSendableMetatype<T: SendableMetatype>(_: T.Type) {}
  func acceptsSendableMetatypeStrict<T: SendableMetatype>(_: T.Type) {}

  func testWarning<T>(t: T.Type) { // expected-note 2 {{consider making generic parameter 'T' conform to the 'SendableMetatype' protocol}} {{21-21=: SendableMetatype}}
    acceptsSendableMetatype(t) // expected-warning {{type 'T' does not conform to the 'SendableMetatype' protocol}}
    acceptsSendableMetatypeStrict(t) // expected-error {{type 'T' does not conform to the 'SendableMetatype' protocol}}
  }

  func testOK<T: P>(t: T.Type) {
    acceptsSendableMetatype(t) // Ok (because P is `Sendable`
  }
}
