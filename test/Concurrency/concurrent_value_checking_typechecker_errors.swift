// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify

// This file contains tests that test specific type checker functionality that
// used to be in concurrent_value_checking.swift. This prevented other region
// isolation errors from being emitted. We used to use multiple prefix tests to
// validate aboth at the same time. Better to just split it.

// REQUIRES: concurrency
// REQUIRES: asserts

class NotConcurrent { } // expected-note 3{{class 'NotConcurrent' does not conform to the 'Sendable' protocol}}

@MainActor var booleanValue: Bool { false }

actor TestActor {}

@globalActor
struct SomeGlobalActor {
  static var shared: TestActor { TestActor() }
}

@SomeGlobalActor
let globalValue: NotConcurrent? = nil

@SomeGlobalActor
func globalSync(_: NotConcurrent?) {
}

@SomeGlobalActor
func globalAsync(_: NotConcurrent?) async {
  if await booleanValue {
    return
  }
  await globalAsync(globalValue) // both okay because we're in the actor
  globalSync(nil)
}

enum E {
  @SomeGlobalActor static let notSafe: NotConcurrent? = nil
}

func globalTest() async {
  // expected-warning@+1 {{global actor 'SomeGlobalActor'-isolated let 'globalValue' cannot be accessed from outside of the actor}} {{11-11=await }}
  let a = globalValue // expected-warning{{non-Sendable type 'NotConcurrent?' of let 'globalValue' cannot exit global actor 'SomeGlobalActor'-isolated context}}
  await globalAsync(a)
  await globalSync(a)

  // expected-warning@+1 {{global actor 'SomeGlobalActor'-isolated static property 'notSafe' cannot be accessed from outside of the actor}} {{11-11=await }}
  let _ = E.notSafe // expected-warning{{non-Sendable type 'NotConcurrent?' of static property 'notSafe' cannot exit global actor 'SomeGlobalActor'-isolated context}}

  // expected-error@+3 {{expression is 'async' but is not marked with 'await'}}
  // expected-note@+2 {{call is 'async'}}
  // expected-note@+1 {{property access is 'async'}}
  globalAsync(E.notSafe)

  // expected-warning@-2 {{non-Sendable type 'NotConcurrent?' of static property 'notSafe' cannot exit global actor 'SomeGlobalActor'-isolated context}}
}

class C11 { }

class C12: @unchecked C11 { } // expected-error {{'@unchecked' cannot apply to non-protocol type 'C11'}}

protocol P { }

protocol Q: @unchecked Sendable { } // expected-error {{'@unchecked' only applies in inheritance clauses}}

typealias TypeAlias1 = @unchecked P // expected-error {{'@unchecked' only applies in inheritance clauses}}
