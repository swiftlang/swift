// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-flow-sensitive-concurrent-captures
// REQUIRES: concurrency

// Concurrent attribute on a function type.
// expected-note@+1{{found this candidate}}
func f(_ fn: @Sendable (Int) -> Int) { }

// Okay to overload @Sendable vs. not concurrent
// expected-note@+1{{found this candidate}}
func f(_ fn: (Int) -> Int) { }

// Concurrent attribute with other function attributes.
func onEscaping(_ fn: @escaping @Sendable (Int) -> Int) { }
func onEscaping2(_ fn: @Sendable @escaping (Int) -> Int) { }
func onAutoclosure(_ fn: @autoclosure @Sendable () -> Int) { }
func onAutoclosure2(_ fn: @Sendable @autoclosure () -> Int) { }
func onEscapingAutoclosure(_ fn: @Sendable @autoclosure @escaping () -> Int) { }
func onEscapingAutoclosure2(_ fn: @escaping @autoclosure @Sendable () -> Int) { }

func acceptsConcurrent(_ fn: @Sendable (Int) -> Int) { }
func acceptsNonConcurrent(_ fn: (Int) -> Int) { }

@Sendable func negate(_ x: Int) -> Int { -x }

func passingConcurrentOrNot(
  _ cfn: @Sendable (Int) -> Int,
  ncfn: (Int) -> Int // expected-note{{parameter 'ncfn' is implicitly non-Sendable}}{{9-9=@Sendable }}
) {
  // Ambiguous because preconcurrency code doesn't consider `@Sendable`.
  f(cfn) // expected-error{{ambiguous use of 'f'}}

  // Okay due to overloading
  f(ncfn)

  acceptsConcurrent(cfn) // okay
  acceptsConcurrent(ncfn) // expected-warning{{passing non-Sendable parameter 'ncfn' to function expecting a '@Sendable' closure}}
  acceptsNonConcurrent(cfn) // okay
  acceptsNonConcurrent(ncfn) // okay

  acceptsConcurrent(negate)
  acceptsNonConcurrent(negate)

  let _: Int = negate // expected-error{{cannot convert value of type '@Sendable (Int) -> Int' to specified type 'Int'}}
}

func closures() {
  // Okay, inferring @Sendable
  acceptsConcurrent { $0 }
  acceptsConcurrent({ $0 })
  acceptsConcurrent({ i in i })
  acceptsConcurrent({ (i: Int) -> Int in
      print(i)
      return i
    })

  let closure1 = { $0 + 1 } // inferred to be non-Sendable
  acceptsConcurrent(closure1) // expected-warning{{converting non-Sendable function value to '@Sendable (Int) -> Int' may introduce data races}}
}

// Mutation of captured locals from within @Sendable functions.
extension Int {
  mutating func makeNegative() {
    self = -self
  }

  func printMe() {
    print(self)
  }
}

func mutationOfLocal() {
  var localInt = 17
  acceptsConcurrent { i in
    // Non-mutating accesses are okay
    print(localInt + 17)
    localInt.printMe()

    // Mutations of locally-defined variables are fine.
    var localResult = localInt + 1
    print(localResult)

    _ = {
      localResult = localResult + 1
    }()

    // Mutations of captured variables executing concurrently are bad.
    localInt = 17 // expected-warning{{mutation of captured var 'localInt' in concurrently-executing code}}
    localInt += 1 // expected-warning{{mutation of captured var 'localInt' in concurrently-executing code}}
    localInt.makeNegative() // expected-warning{{mutation of captured var 'localInt' in concurrently-executing code}}

    _ = {
      localInt = localInt + 12 // expected-warning{{mutation of captured var 'localInt' in concurrently-executing code}}
    }()

    return i + localInt
  }

  localInt = 20
}

struct NonTrivialValueType {
  var int: Int = 0
  var array: [Int] = []
  var optArray: [Int]? = nil
}

func testCaseNonTrivialValue() {
  var i = NonTrivialValueType()
  var j = 0
  acceptsConcurrent { value in
    print(i.int)
    print(i.array[0])
    print(i.array[j])
    print(i.optArray?[j] ?? 0)
    print(i.optArray![j])

    i.int = 5 // expected-warning{{mutation of captured var 'i' in concurrently-executing code}}
    i.array[0] = 5 // expected-warning{{mutation of captured var 'i' in concurrently-executing code}}

    return value
  }

  j = 17
}

func testExplicitConcurrentClosure() {
  let fn = { @Sendable in
    17
  }
  let _: String = fn // expected-error{{cannot convert value of type '@Sendable () -> Int' to specified type 'String'}}
}

class SuperSendable {
  func runsInBackground(_: @Sendable () -> Void) {}
  func runsInForeground(_: () -> Void) {} // expected-note {{overridden declaration is here}}
  func runnableInBackground() -> @Sendable () -> Void { fatalError() } // expected-note {{overridden declaration is here}}
  func runnableInForeground() -> () -> Void { fatalError() }
}

class SubSendable: SuperSendable {
  override func runsInBackground(_: () -> Void) {}
  override func runsInForeground(_: @Sendable () -> Void) {} // expected-warning {{declaration 'runsInForeground' has a type with different sendability from any potential overrides}}
  override func runnableInBackground() -> () -> Void { fatalError() }  // expected-warning {{declaration 'runnableInBackground()' has a type with different sendability from any potential overrides}}
  override func runnableInForeground() -> @Sendable () -> Void { fatalError() }
}

protocol AbstractSendable {
  func runsInBackground(_: @Sendable () -> Void)
  func runsInForeground(_: () -> Void) // expected-note {{expected sendability to match requirement here}}
  func runnableInBackground() -> @Sendable () -> Void // expected-note {{expected sendability to match requirement here}}
  func runnableInForeground() -> () -> Void
}

struct ConcreteSendable: AbstractSendable {
  func runsInBackground(_: () -> Void) {}
  func runsInForeground(_: @Sendable () -> Void) {} // expected-warning {{sendability of function types in instance method 'runsInForeground' does not match requirement in protocol 'AbstractSendable'}}
  func runnableInBackground() -> () -> Void { fatalError() } // expected-warning {{sendability of function types in instance method 'runnableInBackground()' does not match requirement in protocol 'AbstractSendable'}}
  func runnableInForeground() -> @Sendable () -> Void { fatalError() }
}
