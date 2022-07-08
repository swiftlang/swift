// RUN: %target-typecheck-verify-swift  -disable-availability-checking
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
  let _: Int = unsafelySendableClosure // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = unsafelyMainActorClosure // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = unsafelyDoEverythingClosure // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = x.unsafelyDoEverythingClosure // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = X.unsafelyDoEverythingClosure // expected-error{{type '(X) -> (() -> Void) -> ()'}}
  let _: Int = (X.unsafelyDoEverythingClosure)(x) // expected-error{{type '(() -> Void) -> ()'}}
  let _: Int = x.sendableVar // expected-error{{type '() -> Void'}}
  let _: Int = x.mainActorVar // expected-error{{type '() -> Void'}}
  let _: Int = x[{ onMainActor() }] // expected-error{{type '() -> Void'}}
  let _: Int = X[statically: { onMainActor() }] // expected-error{{type '() -> Void'}}
}

@MainActor @preconcurrency func onMainActorAlways() { }

@preconcurrency @MainActor class MyModelClass {
  func f() { }
}

func testCalls(x: X) {
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

  onMainActorAlways() // okay, haven't adopted concurrency

  let _: () -> Void = onMainActorAlways

  // both okay
  let c = MyModelClass()
  c.f()
}

func testCallsWithAsync() async {
  onMainActorAlways() // expected-error{{expression is 'async' but is not marked with 'await'}}
  // expected-note@-1{{calls to global function 'onMainActorAlways()' from outside of its actor context are implicitly asynchronous}}

  let _: () -> Void = onMainActorAlways // expected-warning{{converting function value of type '@MainActor () -> ()' to '() -> Void' loses global actor 'MainActor'}}

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

struct S1: P {
  var ns: NS
}

struct S2: Q {
  var ns: NS
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
  let _: Int = fn1   // expected-error{{cannot convert value of type '([String]) -> Int' to specified type 'Int'}}

  let fn2 = StringPlacement.position(before:)
  let _: Int = fn2 // expected-error{{cannot convert value of type '(String) -> ([String]) -> Int' to specified type 'Int'}}
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

class C {
  var ev: EventLoop? = nil

  func test(i: Int) {
    func doNext() {
      doPreconcurrency {
        self.ev?.scheduleTask(deadline: i, doNext)
        return
      }
    }
  }
}

