// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -verify-additional-prefix minimal-targeted-
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted -verify-additional-prefix minimal-targeted-
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -verify-additional-prefix complete-sns-
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-experimental-feature SendNonSendable -verify-additional-prefix complete-sns-

// REQUIRES: concurrency
// REQUIRES: asserts

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

func testInAsync(x: X, plainClosure: () -> Void) async { // expected-note 2{{parameter 'plainClosure' is implicitly non-sendable}}
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

  unsafelySendableClosure(plainClosure) // expected-warning {{passing non-sendable parameter 'plainClosure' to function expecting a @Sendable closure}}
  unsafelyMainActorClosure(plainClosure)
  unsafelyDoEverythingClosure(plainClosure) // expected-warning {{passing non-sendable parameter 'plainClosure' to function expecting a @Sendable closure}}
}

func testElsewhere(x: X) {
  let _: Int = unsafelySendableClosure // expected-minimal-targeted-error {{type '(() -> Void) -> ()'}}
  // expected-complete-sns-error @-1 {{type '(@Sendable () -> Void) -> ()'}}
  let _: Int = unsafelyMainActorClosure // expected-minimal-targeted-error {{type '(() -> Void) -> ()'}}
  // expected-complete-sns-error @-1 {{type '(@MainActor () -> Void) -> ()'}}
  let _: Int = unsafelyDoEverythingClosure // expected-minimal-targeted-error {{type '(() -> Void) -> ()'}}
  // expected-complete-sns-error @-1 {{type '(@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = x.unsafelyDoEverythingClosure // expected-minimal-targeted-error{{type '(() -> Void) -> ()'}}
  // expected-complete-sns-error @-1 {{type '(@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = X.unsafelyDoEverythingClosure // expected-minimal-targeted-error{{type '(X) -> (() -> Void) -> ()'}}
  // expected-complete-sns-error @-1 {{type '(X) -> (@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = (X.unsafelyDoEverythingClosure)(x) // expected-minimal-targeted-error{{type '(() -> Void) -> ()'}}
  // expected-complete-sns-error @-1 {{type '(@MainActor @Sendable () -> Void) -> ()'}}
  let _: Int = x.sendableVar // expected-minimal-targeted-error {{type '() -> Void'}}
  // expected-complete-sns-error @-1 {{type '@Sendable () -> Void'}}
  let _: Int = x.mainActorVar // expected-minimal-targeted-error {{type '() -> Void'}}
  // expected-complete-sns-error @-1 {{type '@MainActor () -> Void'}}
  let _: Int = x[{ onMainActor() }] // expected-minimal-targeted-error {{type '() -> Void'}}
  // expected-complete-sns-error @-1 {{type '@Sendable () -> Void'}}
  let _: Int = X[statically: { onMainActor() }] // expected-minimal-targeted-error {{type '() -> Void'}}
  // expected-complete-sns-error @-1 {{type '@Sendable () -> Void'}}
}

@MainActor @preconcurrency func onMainActorAlways() { }
// expected-complete-sns-note @-1 {{calls to global function 'onMainActorAlways()' from outside of its actor context are implicitly asynchronous}}

@preconcurrency @MainActor class MyModelClass {
  // expected-complete-sns-note @-1 {{calls to initializer 'init()' from outside of its actor context are implicitly asynchronous}}
  func f() { }
  // expected-complete-sns-note @-1 {{calls to instance method 'f()' from outside of its actor context are implicitly asynchronous}}
}

func testCalls(x: X) {
  // expected-complete-sns-note @-1 3{{add '@MainActor' to make global function 'testCalls(x:)' part of global actor 'MainActor'}}
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
  // expected-complete-sns-warning @-1 {{call to main actor-isolated global function 'onMainActorAlways()' in a synchronous nonisolated context}}

  // Ok with minimal/targeted concurrency, Not ok with complete.
  let _: () -> Void = onMainActorAlways // expected-complete-sns-warning {{converting function value of type '@MainActor () -> ()' to '() -> Void' loses global actor 'MainActor'}}

  // both okay with minimal/targeted... an error with complete.
  let c = MyModelClass() // expected-complete-sns-error {{call to main actor-isolated initializer 'init()' in a synchronous nonisolated context}}
  c.f() // expected-complete-sns-error {{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
}

func testCallsWithAsync() async {
  onMainActorAlways() // expected-error{{expression is 'async' but is not marked with 'await'}}
  // expected-note@-1{{calls to global function 'onMainActorAlways()' from outside of its actor context are implicitly asynchronous}}

  let _: () -> Void = onMainActorAlways // expected-warning {{converting function value of type '@MainActor () -> ()' to '() -> Void' loses global actor 'MainActor'}}

  let c = MyModelClass() // expected-error{{expression is 'async' but is not marked with 'await'}}
  // expected-note@-1{{calls to initializer 'init()' from outside of its actor context are implicitly asynchronous}}
  c.f() // expected-error{{expression is 'async' but is not marked with 'await'}}
  // expected-note@-1{{calls to instance method 'f()' from outside of its actor context are implicitly asynchronous}}
}

// ---------------------------------------------------------------------------
// Protocols that inherit Sendable and predate concurrency.
// ---------------------------------------------------------------------------
@preconcurrency protocol P: Sendable { }
protocol Q: P { }

class NS { } // expected-note{{class 'NS' does not conform to the 'Sendable' protocol}}
// expected-complete-sns-note @-1 2{{class 'NS' does not conform to the 'Sendable' protocol}}

struct S1: P {
  var ns: NS // expected-complete-sns-warning {{stored property 'ns' of 'Sendable'-conforming struct 'S1' has non-sendable type 'NS'}}
}

struct S2: Q {
  var ns: NS // expected-complete-sns-warning {{stored property 'ns' of 'Sendable'-conforming struct 'S2' has non-sendable type 'NS'}}
}

struct S3: Q, Sendable {
  var ns: NS // expected-warning{{stored property 'ns' of 'Sendable'-conforming struct 'S3' has non-sendable type 'NS'}}
}

// ---------------------------------------------------------------------------
// Historical attribute names do nothing (but are permitted)
// ---------------------------------------------------------------------------
func aFailedExperiment(@_unsafeSendable _ body: @escaping () -> Void) { }
// expected-warning@-1{{'_unsafeSendable' attribute has been removed in favor of @preconcurrency}}

func anothingFailedExperiment(@_unsafeMainActor _ body: @escaping () -> Void) { }
// expected-warning@-1{{'_unsafeMainActor' attribute has been removed in favor of @preconcurrency}}

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
  // expected-complete-sns-error @-1 {{type 'StringPlacement.StringPosition' (aka '@Sendable (Array<String>) -> Int')}}

  let fn2 = StringPlacement.position(before:)
  let _: Int = fn2 // expected-minimal-targeted-error{{cannot convert value of type '(String) -> ([String]) -> Int' to specified type 'Int'}}
  // expected-complete-sns-error @-1 {{type '(String) -> StringPlacement.StringPosition' (aka '(String) -> @Sendable (Array<String>) -> Int')}}
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

class C { // expected-complete-sns-note {{'C' does not conform to the 'Sendable' protocol}}
  var ev: EventLoop? = nil

  func test(i: Int) {
    func doNext() { // expected-complete-sns-warning {{concurrently-executed local function 'doNext()' must be marked as '@Sendable'}}
      doPreconcurrency {
        self.ev?.scheduleTask(deadline: i, doNext)
        // expected-complete-sns-warning @-1 {{capture of 'self' with non-sendable type 'C' in a `@Sendable` closure}}
        // expected-complete-sns-warning @-2 {{converting non-sendable function value to '@Sendable () throws -> ()' may introduce data races}}
        // expected-complete-sns-warning @-3 {{capture of 'doNext()' with non-sendable type '() -> ()' in a `@Sendable` closure}}
        // expected-complete-sns-note @-4 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
        return
      }
    }
  }
}

