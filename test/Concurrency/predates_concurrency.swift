// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -verify-additional-prefix minimal-targeted-
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted -verify-additional-prefix minimal-targeted-
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -verify-additional-prefix complete-tns-
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation -verify-additional-prefix complete-tns-

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

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

func testInAsync(x: X, plainClosure: () -> Void) async { // expected-note 2{{parameter 'plainClosure' is implicitly non-Sendable}}
  let _: Int = unsafelySendableClosure // expected-error{{type '(@Sendable () -> Void) -> ()'}}
  let _: Int = unsafelyMainActorClosure // expected-error{{type '(@MainActor () -> Void) -> ()'}}
  let _: Int = unsafelyDoEverythingClosure // expected-error{{type '(@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = x.unsafelyDoEverythingClosure // expected-error{{type '(@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = X.unsafelyDoEverythingClosure // expected-error{{type '(X) -> (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = (X.unsafelyDoEverythingClosure)(x) // expected-error{{type '(@MainActor @Sendable () -> Void) -> ()'}}

  let _: Int = x.sendableVar // expected-error{{type '@Sendable () -> Void'}}
  let _: Int = x.mainActorVar // expected-error{{type '@MainActor () -> Void'}}

  let _: Int = x[{ onMainActor() }] // expected-error{{type '@Sendable () -> Void'}}
  let _: Int = X[statically: { onMainActor() }] // expected-error{{type '@Sendable () -> Void'}}

  unsafelySendableClosure(plainClosure) // expected-warning {{passing non-Sendable parameter 'plainClosure' to function expecting a '@Sendable' closure}}
  unsafelyMainActorClosure(plainClosure)
  unsafelyDoEverythingClosure(plainClosure) // expected-warning {{passing non-Sendable parameter 'plainClosure' to function expecting a '@Sendable' closure}}
}

func testElsewhere(x: X) {
  let _: Int = unsafelySendableClosure // expected-minimal-targeted-error {{type '(() -> Void) -> ()'}}
  // expected-complete-tns-error @-1 {{type '(@Sendable () -> Void) -> ()'}}
  let _: Int = unsafelyMainActorClosure // expected-minimal-targeted-error {{type '(() -> Void) -> ()'}}
  // expected-complete-tns-error @-1 {{type '(@MainActor () -> Void) -> ()'}}
  let _: Int = unsafelyDoEverythingClosure // expected-minimal-targeted-error {{type '(() -> Void) -> ()'}}
  // expected-complete-tns-error @-1 {{type '(@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = x.unsafelyDoEverythingClosure // expected-minimal-targeted-error{{type '(() -> Void) -> ()'}}
  // expected-complete-tns-error @-1 {{type '(@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = X.unsafelyDoEverythingClosure // expected-minimal-targeted-error{{type '(X) -> (() -> Void) -> ()'}}
  // expected-complete-tns-error @-1 {{type '(X) -> (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = (X.unsafelyDoEverythingClosure)(x) // expected-minimal-targeted-error{{type '(() -> Void) -> ()'}}
  // expected-complete-tns-error @-1 {{type '(@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = x.sendableVar // expected-minimal-targeted-error {{type '() -> Void'}}
  // expected-complete-tns-error @-1 {{type '@Sendable () -> Void'}}
  let _: Int = x.mainActorVar // expected-minimal-targeted-error {{type '() -> Void'}}
  // expected-complete-tns-error @-1 {{type '@MainActor () -> Void'}}
  let _: Int = x[{ onMainActor() }] // expected-minimal-targeted-error {{type '() -> Void'}}
  // expected-complete-tns-error @-1 {{type '@Sendable () -> Void'}}
  let _: Int = X[statically: { onMainActor() }] // expected-minimal-targeted-error {{type '() -> Void'}}
  // expected-complete-tns-error @-1 {{type '@Sendable () -> Void'}}
}

@MainActor @preconcurrency func onMainActorAlways() { }
// expected-complete-tns-note @-1 {{calls to global function 'onMainActorAlways()' from outside of its actor context are implicitly asynchronous}}

@preconcurrency @MainActor class MyModelClass {

  // default init() is 'nonisolated' in '-strict-concurrency=complete'

  func f() { }
  // expected-complete-tns-note @-1 {{calls to instance method 'f()' from outside of its actor context are implicitly asynchronous}}
}

func testCalls(x: X) {
  // expected-complete-tns-note @-1 2{{add '@MainActor' to make global function 'testCalls(x:)' part of global actor 'MainActor'}}
  unsafelyMainActorClosure {
    onMainActor()
  }

  unsafelyDoEverythingClosure {
    onMainActor()
  }

  x.unsafelyDoEverythingClosure {
    onMainActor()
    onMainActorAlways()
  }
  (X.unsafelyDoEverythingClosure)(x)( {
    onMainActor()
    })

  onMainActorAlways() // okay with minimal/targeted concurrency. Not ok with complete.
  // expected-complete-tns-warning @-1 {{call to main actor-isolated global function 'onMainActorAlways()' in a synchronous nonisolated context}}

  // Ok with minimal/targeted concurrency, Not ok with complete.
  let _: () -> Void = onMainActorAlways // expected-complete-tns-warning {{converting function value of type '@MainActor () -> ()' to '() -> Void' loses global actor 'MainActor'}}

  let c = MyModelClass()

  // okay with minimal/targeted... an error with complete.
  c.f() // expected-complete-tns-warning {{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
}

func testCallsWithAsync() async {
  onMainActorAlways() // expected-warning{{main actor-isolated global function 'onMainActorAlways()' cannot be called from outside of the actor}} {{3-3=await }}

  let _: () -> Void = onMainActorAlways // expected-warning {{converting function value of type '@MainActor () -> ()' to '() -> Void' loses global actor 'MainActor'}}

  let c = MyModelClass() // expected-minimal-targeted-warning{{main actor-isolated initializer 'init()' cannot be called from outside of the actor}} {{11-11=await }}

  c.f() // expected-warning{{main actor-isolated instance method 'f()' cannot be called from outside of the actor}} {{3-3=await }}
}

// ---------------------------------------------------------------------------
// Protocols that inherit Sendable and predate concurrency.
// ---------------------------------------------------------------------------
@preconcurrency protocol P: Sendable { }
protocol Q: P { }

class NS { } // expected-note{{class 'NS' does not conform to the 'Sendable' protocol}}
// expected-complete-tns-note @-1 2{{class 'NS' does not conform to the 'Sendable' protocol}}

struct S1: P {
  var ns: NS // expected-complete-tns-warning {{stored property 'ns' of 'Sendable'-conforming struct 'S1' has non-Sendable type 'NS'}}
}

struct S2: Q {
  var ns: NS // expected-complete-tns-warning {{stored property 'ns' of 'Sendable'-conforming struct 'S2' has non-Sendable type 'NS'}}
}

struct S3: Q, Sendable {
  var ns: NS // expected-warning{{stored property 'ns' of 'Sendable'-conforming struct 'S3' has non-Sendable type 'NS'}}
}

// ---------------------------------------------------------------------------
// Historical attribute names do nothing (but are permitted)
// ---------------------------------------------------------------------------
func aFailedExperiment(@_unsafeSendable _ body: @escaping () -> Void) { }
// expected-warning@-1{{'_unsafeSendable' attribute has been removed in favor of '@preconcurrency'}}

func anothingFailedExperiment(@_unsafeMainActor _ body: @escaping () -> Void) { }
// expected-warning@-1{{'_unsafeMainActor' attribute has been removed in favor of '@preconcurrency'}}

// ---------------------------------------------------------------------------
// Random bugs
// ---------------------------------------------------------------------------

public enum StringPlacement : Sendable {
  public typealias StringPosition = @Sendable (_: [String]) -> Int

  @preconcurrency
  public static func position(before string: String) -> StringPosition {
    return { _ in 0 }
  }

  @preconcurrency
  public static func position(after string: String) -> StringPosition {
    return { _ in 0 }
  }
}

func testStringPlacement() {
  let fn1 = StringPlacement.position(before: "Test")
  let _: Int = fn1   // expected-minimal-targeted-error{{cannot convert value of type '([String]) -> Int' to specified type 'Int'}}
  // expected-complete-tns-error @-1 {{type 'StringPlacement.StringPosition' (aka '@Sendable (Array<String>) -> Int')}}

  let fn2 = StringPlacement.position(before:)
  let _: Int = fn2 // expected-minimal-targeted-error{{cannot convert value of type '(String) -> ([String]) -> Int' to specified type 'Int'}}
  // expected-complete-tns-error @-1 {{type '(String) -> StringPlacement.StringPosition' (aka '(String) -> @Sendable (Array<String>) -> Int')}}
}

// @preconcurrency in an outer closure
// (https://github.com/apple/swift/issues/59910)
struct Scheduled<T> { }

@preconcurrency
func doPreconcurrency(_: @Sendable () -> Void) { }

class EventLoop {
  @discardableResult
  @preconcurrency
  func scheduleTask<T>(deadline: Int, _ task: @escaping @Sendable () throws -> T) -> Scheduled<T> { fatalError("") }
}

class C { // expected-complete-tns-note {{'C' does not conform to the 'Sendable' protocol}}
  var ev: EventLoop? = nil

  func test(i: Int) {
    func doNext() { // expected-complete-tns-warning {{concurrently-executed local function 'doNext()' must be marked as '@Sendable'}}
      doPreconcurrency {
        self.ev?.scheduleTask(deadline: i, doNext)
        // expected-complete-tns-warning @-1 {{capture of 'self' with non-Sendable type 'C' in a '@Sendable' closure}}
        // expected-complete-tns-warning @-2 {{converting non-Sendable function value to '@Sendable () throws -> ()' may introduce data races}}
        // expected-complete-tns-warning @-3 {{capture of 'doNext()' with non-Sendable type '() -> ()' in a '@Sendable' closure}}
        // expected-complete-tns-note @-4 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
        return
      }
    }
  }
}

@preconcurrency @MainActor
class MainActorPreconcurrency {}

class InferMainActorPreconcurrency: MainActorPreconcurrency {
  static func predatesConcurrency() {}
  // expected-note@-1 {{calls to static method 'predatesConcurrency()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-2 {{main actor isolation inferred from inheritance from class 'MainActorPreconcurrency'}}
}

nonisolated func blah() {
  InferMainActorPreconcurrency.predatesConcurrency()
  // expected-warning@-1 {{call to main actor-isolated static method 'predatesConcurrency()' in a synchronous nonisolated context}}
}

protocol NotIsolated {
  func requirement()
}

// expected-complete-tns-warning@+1{{conformance of 'MainActorPreconcurrency' to protocol 'NotIsolated' crosses into main actor-isolated code and can cause data races}}
extension MainActorPreconcurrency: NotIsolated {
  // expected-complete-note@-1{{add '@preconcurrency' to the 'NotIsolated' conformance to suppress isolation-related diagnostics}}{{36-36=@preconcurrency }}
  // expected-complete-tns-note@-2{{turn data races into runtime errors with '@preconcurrency'}}{{36-36=@preconcurrency }}
  // expected-complete-tns-note@-3{{mark all declarations used in the conformance 'nonisolated'}}
  // expected-complete-tns-note@-4{{isolate this conformance to the main actor with '@MainActor'}}
  func requirement() {}
  // expected-complete-tns-note@-1 {{main actor-isolated instance method 'requirement()' cannot satisfy nonisolated requirement}}
  // expected-complete-tns-note@-2 {{calls to instance method 'requirement()' from outside of its actor context are implicitly asynchronous}}


  class Nested {
    weak var c: MainActorPreconcurrency?

    func test() {
    // expected-complete-tns-note@-1 {{add '@MainActor' to make instance method 'test()' part of global actor 'MainActor'}}

      if let c {
        c.requirement()
        // expected-complete-tns-warning@-1 {{call to main actor-isolated instance method 'requirement()' in a synchronous nonisolated context}}
      }
    }
  }
}

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

    open var test6: (@MainActor () -> Void)? { // expected-note {{attempt to override property here}}
      nil
    }

    @preconcurrency
    func test7(_: (@MainActor () -> Void)? = nil) { // expected-note {{overridden declaration is here}}
    }

    init() {
      self.test1 = nil
      self.test2 = [:]
      self.test3 = 42
    }
  }

  class Test : Base {
    override var test1: [Any]? {
      // expected-warning@-1 {{declaration 'test1' has a type with different sendability from any potential overrides; this is an error in the Swift 6 language mode}}
      get { nil }
      set { }
    }

    override var test2: [String: [Int: Any]] {
      // expected-warning@-1 {{declaration 'test2' has a type with different sendability from any potential overrides; this is an error in the Swift 6 language mode}}
      get { [:] }
      set {}
    }

    override var test3: Any {
      // expected-warning@-1 {{declaration 'test3' has a type with different sendability from any potential overrides; this is an error in the Swift 6 language mode}}
      get { 42 }
      set { }
    }

    override var test4: (((any Sendable)?) -> Void)? {
      // expected-warning@-1 {{declaration 'test4' has a type with different sendability from any potential overrides; this is an error in the Swift 6 language mode}}
      nil
    }

    override var test5: (() -> Void)? {
      // expected-warning@-1 {{declaration 'test5' has a type with different global actor isolation from any potential overrides; this is an error in the Swift 6 language mode}}
      nil
    }

    override var test6: (() -> Void)? {
      // expected-error@-1 {{property 'test6' with type '(() -> Void)?' cannot override a property with type '(@MainActor () -> Void)?'}}
      nil
    }

    override func test7(_: (() -> Void)?) {
      // expected-warning@-1 {{declaration 'test7' has a type with different global actor isolation from any potential overrides; this is an error in the Swift 6 language mode}}
    }
  }
}

// rdar://132700409 - coercion PartialKeyPath & Sendable -> PartialKeyPath crashes in CSApply
do {
  struct Test {
    enum KeyPath {
      static var member: PartialKeyPath<Test> {
        fatalError()
      }
    }
  }

  struct KeyPathComparator<Compared> {
    @preconcurrency public let keyPath: any PartialKeyPath<Compared> & Sendable

    func testDirect() {
      switch keyPath { // Ok
      case Test.KeyPath.member: break // Ok
      default: break
      }
    }

    func testErasure() {
      let kp: PartialKeyPath<Compared> = keyPath
      switch kp { // Ok
      case Test.KeyPath.member: break // Ok
      default: break
      }
    }
  }
}

func testSendableMetatypeDowngrades() {
  @preconcurrency
  func acceptsSendableMetatype<T: SendableMetatype>(_: T.Type) {}
  func acceptsSendableMetatypeStrict<T: SendableMetatype>(_: T.Type) {}

  func test<T>(t: T.Type) { // expected-complete-tns-note 2 {{consider making generic parameter 'T' conform to the 'SendableMetatype' protocol}} {{14-14=: SendableMetatype}}
    acceptsSendableMetatype(t)
    // expected-complete-tns-warning@-1 {{type 'T' does not conform to the 'SendableMetatype' protocol}}
    acceptsSendableMetatypeStrict(t)
    // expected-complete-tns-warning@-1 {{type 'T' does not conform to the 'SendableMetatype' protocol}}
  }
}

do {
  func test(@_inheritActorContext _: @Sendable () -> Void) async {
    // expected-warning@-1 {{@_inheritActorContext only applies to '@isolated(any)' parameters or parameters with asynchronous function types; this will be an error in a future Swift language mode}}
  }

  @MainActor
  @preconcurrency
  class Test {
    struct V {}

    var value: V? // expected-note {{property declared here}}

    func run() async {
      await test {
        if let value {
          // expected-warning@-1 {{main actor-isolated property 'value' can not be referenced from a Sendable closure; this is an error in the Swift 6 language mode}}
          print(value)
        }
      }
    }
  }
}
