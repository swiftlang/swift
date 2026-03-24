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

typealias IntFn = () -> Int
// expected-note@-1{{add '@Sendable' to the definition of 'IntFn' (aka '() -> Int')}}{{1-1=@preconcurrency }}{{19-19=@Sendable }}{{+4:20-30=}}
// expected-note@-2{{add '@Sendable' to the definition of 'IntFn' (aka '() -> Int')}}{{1-1=@preconcurrency }}{{19-19=@Sendable }}{{+6:38-48=}}
// expected-note@-3{{add '@Sendable' to the definition of 'IntFn' (aka '() -> Int')}}{{1-1=@preconcurrency }}{{19-19=@Sendable }}{{+8:29-39=}}
func onAlias(_ fn: @Sendable IntFn) { } // expected-error@:20{{attribute '@Sendable' cannot be applied to a type alias}}
// expected-note@-1{{expand 'IntFn' (aka '() -> Int') to apply '@Sendable'}}{{30-35=() -> Int}}
func onEscapingAlias(_ fn: @escaping @Sendable IntFn) { } // expected-warning@:38{{attribute '@Sendable' cannot be applied to a type alias}}
// expected-note@-1{{expand 'IntFn' (aka '() -> Int') to apply '@Sendable'}}{{48-53=() -> Int}}
func onEscapingAlias2(_ fn: @Sendable @escaping IntFn) { } // expected-error@:29{{attribute '@Sendable' cannot be applied to a type alias}}
// expected-note@-1{{expand 'IntFn' (aka '() -> Int') to apply '@Sendable'}}{{49-54=() -> Int}}

typealias VoidFn = () -> Void
// expected-note@-1{{add '@isolated' to the definition of 'VoidFn' (aka '() -> ()')}}{{1-1=@preconcurrency }}{{20-20=@isolated(any) }}{{+3:28-43=}}
// expected-note@-2{{add '@isolated' to the definition of 'VoidFn' (aka '() -> ()')}}{{1-1=@preconcurrency }}{{20-20=@isolated(any) }}{{+5:46-61=}}
func onIsolatedAlias(_ fn: @isolated(any) VoidFn) { } // expected-error@:28{{attribute '@isolated' cannot be applied to a type alias}}
// expected-note@-1{{expand 'VoidFn' (aka '() -> ()') to apply '@isolated'}}{{43-49=() -> Void}}
func onEscapingIsolatedAlias(_ fn: @escaping @isolated(any) VoidFn) { } // expected-warning@:46{{attribute '@isolated' cannot be applied to a type alias}}
// expected-note@-1{{expand 'VoidFn' (aka '() -> ()') to apply '@isolated'}}{{61-67=() -> Void}}

// Alias that already has the attribute should not suggest adding it again.
typealias SendableFn = @Sendable () -> Int
func onAlreadySendable(_ fn: @Sendable SendableFn) { } // expected-error{{attribute '@Sendable' cannot be applied to a type alias}}
// expected-note@-1{{'@Sendable' is redundant on 'SendableFn' (aka '@Sendable () -> Int')}}{{30-40=}}{{none}}

// Multiple function attrs on alias.
typealias PlainFn = () -> ()
// expected-note@-1{{add '@Sendable' to the definition of 'PlainFn' (aka '() -> ()')}}{{1-1=@preconcurrency }}{{21-21=@Sendable }}{{+3:28-38=}}
// expected-note@-2{{add '@isolated' to the definition of 'PlainFn' (aka '() -> ()')}}{{1-1=@preconcurrency }}{{21-21=@isolated(any) }}{{+3:38-53=}}
func onMultipleAttrs(_ fn: @Sendable @isolated(any) PlainFn) { }
// expected-error@-1:28{{attribute '@Sendable' cannot be applied to a type alias}}
// expected-error@-2:38{{attribute '@isolated' cannot be applied to a type alias}}
// expected-note@-3{{expand 'PlainFn' (aka '() -> ()') to apply '@Sendable'}}{{53-60=() -> ()}}
// expected-note@-4{{expand 'PlainFn' (aka '() -> ()') to apply '@isolated'}}{{53-60=() -> ()}}

// @Sendable is error (before claimed @escaping), @isolated(any) is warning (after claimed @escaping).
typealias MixedFn = () -> ()
// expected-note@-1{{add '@Sendable' to the definition of 'MixedFn' (aka '() -> ()')}}{{1-1=@preconcurrency }}{{21-21=@Sendable }}{{+3:25-35=}}
// expected-note@-2{{add '@isolated' to the definition of 'MixedFn' (aka '() -> ()')}}{{1-1=@preconcurrency }}{{21-21=@isolated(any) }}{{+3:45-60=}}
func onMixedAttrs(_ fn: @Sendable @escaping @isolated(any) MixedFn) { }
// expected-error@-1:25{{attribute '@Sendable' cannot be applied to a type alias}}
// expected-warning@-2:45{{attribute '@isolated' cannot be applied to a type alias}}
// expected-note@-3{{expand 'MixedFn' (aka '() -> ()') to apply '@Sendable'}}{{60-67=() -> ()}}
// expected-note@-4{{expand 'MixedFn' (aka '() -> ()') to apply '@isolated'}}{{60-67=() -> ()}}

// Generic alias.
typealias GenericFn<T> = (T) -> T
// expected-note@-1{{add '@Sendable' to the definition of 'GenericFn<Int>' (aka '(Int) -> Int')}}{{1-1=@preconcurrency }}{{26-26=@Sendable }}{{+2:27-37=}}
func onGenericAlias(_ fn: @Sendable GenericFn<Int>) { } // expected-error@:27{{attribute '@Sendable' cannot be applied to a type alias}}
// expected-note@-1{{expand 'GenericFn<Int>' (aka '(Int) -> Int') to apply '@Sendable'}}{{37-51=(Int) -> Int}}

@preconcurrency typealias PreconcurrencyFn = () -> ()
// expected-note@-1{{add '@Sendable' to the definition of 'PreconcurrencyFn' (aka '() -> ()')}}{{46-46=@Sendable }}{{+2:34-44=}}{{none}}
func onPreconcurrencyAlias(_ fn: @Sendable PreconcurrencyFn) { } // expected-error@:34{{attribute '@Sendable' cannot be applied to a type alias}}
// expected-note@-1{{expand 'PreconcurrencyFn' (aka '() -> ()') to apply '@Sendable'}}{{44-60=() -> ()}}

typealias OptionalFn = (() -> ())?
func onOptionalAlias(_ fn: @Sendable OptionalFn) { } // expected-error{{'@Sendable' only applies to function types}}

// Parenthesized function type alias — expansion should drop the parens.
typealias ParenFn = (() -> ())
// expected-note@-1{{add '@Sendable' to the definition of 'ParenFn' (aka '() -> ()')}}{{1-1=@preconcurrency }}{{21-21=@Sendable }}{{+2:25-35=}}
func onParenAlias(_ fn: @Sendable ParenFn) { } // expected-error@:25{{attribute '@Sendable' cannot be applied to a type alias}}
// expected-note@-1{{expand 'ParenFn' (aka '() -> ()') to apply '@Sendable'}}{{35-42=() -> ()}}

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
