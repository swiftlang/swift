// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

// provides coverage for rdar://71548470

actor TestActor {}

@globalActor
struct SomeGlobalActor {
  static var shared: TestActor { TestActor() }
}

// expected-note@+1 13 {{calls to global function 'syncGlobActorFn()' from outside of its actor context are implicitly asynchronous}}
@SomeGlobalActor func syncGlobActorFn() { }
@SomeGlobalActor func asyncGlobalActFn() async { }

actor Alex {
  @SomeGlobalActor let const_memb = 20
  @SomeGlobalActor var mut_memb = 30
  @SomeGlobalActor func method() {} // expected-note 2 {{calls to instance method 'method()' from outside of its actor context are implicitly asynchronous}}

  // expected-note@+1 2 {{mutation of this subscript is only permitted within the actor}}
  @SomeGlobalActor subscript(index : Int) -> Int {
    get {
      return index * 2
    }
    set {}
  }
}

func referenceGlobalActor() async {
  let a = Alex()
  _ = a.method // expected-error {{instance method 'method()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
  _ = a.const_memb
  _ = a.mut_memb  // expected-error{{property access is 'async' but is not marked with 'await'}}

  _ = a[1]  // expected-error{{subscript access is 'async' but is not marked with 'await'}}
  a[0] = 1  // expected-error{{subscript 'subscript(_:)' isolated to global actor 'SomeGlobalActor' can not be mutated from this context}}
}


// expected-note@+1 {{add '@SomeGlobalActor' to make global function 'referenceGlobalActor2()' part of global actor 'SomeGlobalActor'}} {{1-1=@SomeGlobalActor }}
func referenceGlobalActor2() {
  // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
  let x = syncGlobActorFn
  x()
}


// expected-note@+1 {{add 'async' to function 'referenceAsyncGlobalActor()' to make it asynchronous}} {{33-33= async}}
func referenceAsyncGlobalActor() {
  let y = asyncGlobalActFn
  y() // expected-error{{'async' call in a function that does not support concurrency}}
}


// expected-note@+1 {{add '@SomeGlobalActor' to make global function 'callGlobalActor()' part of global actor 'SomeGlobalActor'}} {{1-1=@SomeGlobalActor }}
func callGlobalActor() {
  syncGlobActorFn() // expected-error {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this synchronous context}}
}

func fromClosure() {
  { () -> Void in
    // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from a non-isolated context}}
    let x = syncGlobActorFn
    x()
  }()

  // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from a non-isolated synchronous context}}
  let _ = { syncGlobActorFn() }()
}

class Taylor {
  init() {
    syncGlobActorFn() // expected-error {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this synchronous context}}

    // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
    _ = syncGlobActorFn
  }

  deinit {
    syncGlobActorFn() // expected-error {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this synchronous context}}

    // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
    _ = syncGlobActorFn
  }

  // expected-note@+1 2 {{add '@SomeGlobalActor' to make instance method 'method1()' part of global actor 'SomeGlobalActor'}} {{3-3=@SomeGlobalActor }}
  func method1() {
    syncGlobActorFn() // expected-error {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this synchronous context}}

    // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
    _ = syncGlobActorFn
  }

  // expected-note@+1 2 {{add '@SomeGlobalActor' to make instance method 'cannotBeHandler()' part of global actor 'SomeGlobalActor'}} {{3-3=@SomeGlobalActor }}
  func cannotBeHandler() -> Int {
    syncGlobActorFn() // expected-error {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this synchronous context}}

    // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
    _ = syncGlobActorFn
    return 0
  }
}


func fromAsync() async {
  // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
  let x = syncGlobActorFn
  x()

  let y = asyncGlobalActFn
  y() // expected-error{{call is 'async' but is not marked with 'await'}}

  let a = Alex()
  // expected-error@+1 {{instance method 'method()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
  _ = a.method
  _ = a.const_memb
  _ = a.mut_memb  // expected-error{{property access is 'async' but is not marked with 'await'}}

  _ = a[1]  // expected-error{{subscript access is 'async' but is not marked with 'await'}}
  _ = await a[1]
  a[0] = 1  // expected-error{{subscript 'subscript(_:)' isolated to global actor 'SomeGlobalActor' can not be mutated from this context}}
}

// expected-note@+1{{mutation of this var is only permitted within the actor}}
@SomeGlobalActor var value: Int = 42

func topLevelSyncFunction(_ number: inout Int) { }
// expected-error@+1{{var 'value' isolated to global actor 'SomeGlobalActor' can not be used 'inout' from this context}}
topLevelSyncFunction(&value)
