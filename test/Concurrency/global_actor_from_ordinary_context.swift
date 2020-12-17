// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

// provides coverage for rdar://71548470

actor class TestActor {}

@globalActor
struct SomeGlobalActor {
  static var shared: TestActor { TestActor() }
}

// expected-note@+1 13 {{calls to global function 'syncGlobActorFn()' from outside of its actor context are implicitly asynchronous}}
@SomeGlobalActor func syncGlobActorFn() { }
@SomeGlobalActor func asyncGlobalActFn() async { }

actor class Alex {
  @SomeGlobalActor let const_memb = 20
  @SomeGlobalActor var mut_memb = 30 // expected-note 2 {{mutable state is only available within the actor instance}}
  @SomeGlobalActor func method() {} // expected-note 2 {{calls to instance method 'method()' from outside of its actor context are implicitly asynchronous}}
  @SomeGlobalActor subscript(index : Int) -> Int { // expected-note 4 {{subscript declared here}}
    get {
      return index * 2
    }
    set {}
  }
}


// expected-note@+1 4 {{add '@SomeGlobalActor' to make global function 'referenceGlobalActor()' part of global actor 'SomeGlobalActor'}} {{1-1=@SomeGlobalActor }}
func referenceGlobalActor() {
  let a = Alex()
  // expected-error@+1 {{instance method 'method()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
  _ = a.method
  _ = a.const_memb
  _ = a.mut_memb  // expected-error{{property 'mut_memb' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}

  _ = a[1]  // expected-error{{subscript 'subscript(_:)' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
  a[0] = 1  // expected-error{{subscript 'subscript(_:)' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
}


// expected-note@+1 {{add '@SomeGlobalActor' to make global function 'referenceGlobalActor2()' part of global actor 'SomeGlobalActor'}} {{1-1=@SomeGlobalActor }}
func referenceGlobalActor2() {
  // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
  let x = syncGlobActorFn
  x()
}


// expected-note@+2 {{add '@asyncHandler' to function 'referenceAsyncGlobalActor()' to create an implicit asynchronous context}} {{1-1=@asyncHandler }}
// expected-note@+1 {{add 'async' to function 'referenceAsyncGlobalActor()' to make it asynchronous}} {{none}}
func referenceAsyncGlobalActor() {
  let y = asyncGlobalActFn
  y() // expected-error{{'async' in a function that does not support concurrency}}
}


// expected-note@+3 {{add '@asyncHandler' to function 'callGlobalActor()' to create an implicit asynchronous context}} {{1-1=@asyncHandler }}
// expected-note@+2 {{add 'async' to function 'callGlobalActor()' to make it asynchronous}} {{none}}
// expected-note@+1 {{add '@SomeGlobalActor' to make global function 'callGlobalActor()' part of global actor 'SomeGlobalActor'}} {{1-1=@SomeGlobalActor }}
func callGlobalActor() {
  syncGlobActorFn() // expected-error {{'async' in a function that does not support concurrency}}
}

func fromClosure() {
  { () -> Void in
    // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
    let x = syncGlobActorFn
    x()
  }()

  // expected-error@+2 {{'async' in a function that does not support concurrency}}
  // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
  let _ = { syncGlobActorFn() }()
}

class Taylor {
  init() { // expected-note {{add 'async' to function 'init()' to make it asynchronous}} {{none}}
    syncGlobActorFn() // expected-error {{'async' in a function that does not support concurrency}}

    // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
    _ = syncGlobActorFn
  }

  deinit {
    syncGlobActorFn() // expected-error {{'async' in a function that does not support concurrency}}

    // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
    _ = syncGlobActorFn
  }

  // expected-note@+3 2 {{add '@SomeGlobalActor' to make instance method 'method1()' part of global actor 'SomeGlobalActor'}} {{3-3=@SomeGlobalActor }}
  // expected-note@+2 {{add '@asyncHandler' to function 'method1()' to create an implicit asynchronous context}} {{3-3=@asyncHandler }}
  // expected-note@+1 {{add 'async' to function 'method1()' to make it asynchronous}} {{none}}
  func method1() {
    syncGlobActorFn() // expected-error {{'async' in a function that does not support concurrency}}

    // expected-error@+1 {{global function 'syncGlobActorFn()' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
    _ = syncGlobActorFn
  }

  // expected-note@+2 2 {{add '@SomeGlobalActor' to make instance method 'cannotBeHandler()' part of global actor 'SomeGlobalActor'}} {{3-3=@SomeGlobalActor }}
  // expected-note@+1 {{add 'async' to function 'cannotBeHandler()' to make it asynchronous}}
  func cannotBeHandler() -> Int {
    syncGlobActorFn() // expected-error {{'async' in a function that does not support concurrency}}

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
  _ = a.mut_memb  // expected-error{{property 'mut_memb' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}

  _ = a[1]  // expected-error{{subscript 'subscript(_:)' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
  a[0] = 1  // expected-error{{subscript 'subscript(_:)' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
}

// expected-note@+1{{mutable state is only available within the actor instance}}
@SomeGlobalActor var value: Int = 42

func topLevelSyncFunction(_ number: inout Int) { }
// expected-error@+1{{var 'value' isolated to global actor 'SomeGlobalActor' can not be referenced from this context}}
topLevelSyncFunction(&value)
