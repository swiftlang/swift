// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

// Verify that we don't allow actor-isolated state to be passed via inout
// Check:
//  - can't pass it into a normal async function
//  - can't pass it into a first-class async function as a value
//  - can't pass it into another actor method
//  - can't pass it into a curried/partially applied function
//  - can't pass it inout to a function that doesn't directly touch it
//  - can't pass it into a function that was passed into the calling method
//  - can't call async mutating function on actor isolated state

struct Point {
  var x: Int
  var y: Int

  mutating func setComponents(x: inout Int, y: inout Int) async {
    defer { (x, y) = (self.x, self.y) }
    (self.x, self.y) = (x, y)
  }
}

actor class TestActor {
  var position = Point(x: 0, y: 0)
  var nextPosition = Point(x: 0, y: 1)
  var value1: Int = 0
  var value2: Int = 1
}

func modifyAsynchronously(_ foo: inout Int) async { foo += 1 }
let modifyAsyncValue = modifyAsynchronously

// external function call
extension TestActor {

  // Can't pass actor-isolated primitive into a function
  func inoutAsyncFunctionCall() async {
    // expected-error@+1{{actor-isolated property 'value1' cannot be passed 'inout' to 'async' function call}}
    await modifyAsynchronously(&value1)
  }

  func inoutAsyncClosureCall() async {
    // expected-error@+1{{actor-isolated property 'value1' cannot be passed 'inout' to 'async' function call}}
    await { (_ foo: inout Int) async in foo += 1 }(&value1)
  }

  // Can't pass actor-isolated primitive into first-class function value
  func inoutAsyncValueCall() async {
    // expected-error@+1{{actor-isolated property 'value1' cannot be passed 'inout' to 'async' function call}}
    await modifyAsyncValue(&value1)
  }

  // Can't pass property of actor-isolated state inout to async function
  func inoutPropertyStateValueCall() async {
    // expected-error@+1{{actor-isolated property 'position' cannot be passed 'inout' to 'async' function call}}
    await modifyAsynchronously(&position.x)
  }
}

// internal method call
extension TestActor {
  func modifyByValue(_ other: inout Int) async {
    other += value1
  }

  func passStateIntoMethod() async {
    // expected-error@+1{{actor-isolated property 'value1' cannot be passed 'inout' to 'async' function call}}
    await modifyByValue(&value1)
  }
}

// external class method call
class NonAsyncClass {
  func modifyOtherAsync(_ other : inout Int) async {
    // ...
  }

  func modifyOtherNotAsync(_ other: inout Int) {
    // ...
  }
}

// Calling external class/struct async function
extension TestActor {
  // Can't pass state into async method of another class

  func passStateIntoDifferentClassMethod() async {
    let other = NonAsyncClass()
    let otherCurry = other.modifyOtherAsync
    // expected-error@+1{{actor-isolated property 'value2' cannot be passed 'inout' to 'async' function call}}
    await other.modifyOtherAsync(&value2)
    // expected-error@+1{{actor-isolated property 'value1' cannot be passed 'inout' to 'async' function call}}
    await otherCurry(&value1)
    other.modifyOtherNotAsync(&value2) // This is okay since it's not async!

  }

  func callMutatingFunctionOnStruct() async {
    // expected-error@+3:20{{cannot call mutating async function 'setComponents(x:y:)' on actor-isolated property 'position'}}
    // expected-error@+2:51{{actor-isolated property 'nextPosition' cannot be passed 'inout' to 'async' function call}}
    // expected-error@+1:71{{actor-isolated property 'nextPosition' cannot be passed 'inout' to 'async' function call}}
    await position.setComponents(x: &nextPosition.x, y: &nextPosition.y)

    // expected-error@+3:20{{cannot call mutating async function 'setComponents(x:y:)' on actor-isolated property 'position'}}
    // expected-error@+2:38{{actor-isolated property 'value1' cannot be passed 'inout' to 'async' function call}}
    // expected-error@+1:50{{actor-isolated property 'value2' cannot be passed 'inout' to 'async' function call}}
    await position.setComponents(x: &value1, y: &value2)
  }
}

// Check implicit async testing
actor class DifferentActor {
  func modify(_ state: inout Int) {}
}

extension TestActor {
  func modify(_ state: inout Int) {}

  // Actor state passed inout to implicitly async function on an actor of the
  // same type
  func modifiedByOtherTestActor(_ other: TestActor) async {
    //expected-error@+1{{actor-isolated property 'value2' cannot be passed 'inout' to implicitly 'async' function call}}
    await other.modify(&value2)
  }

  // Actor state passed inout to an implicitly async function on an actor of a
  // different type
  func modifiedByOther(_ other: DifferentActor) async {
    //expected-error@+1{{actor-isolated property 'value2' cannot be passed 'inout' to implicitly 'async' function call}}
    await other.modify(&value2)
  }
}

// Verify global actor protection

@globalActor
struct MyGlobalActor {
  static let shared = TestActor()
}

@MyGlobalActor var number: Int = 0
// expected-note@-1{{var declared here}}
// expected-note@-2{{var declared here}}
// expected-note@-3{{mutable state is only available within the actor instance}}

// expected-error@+2{{actor-isolated var 'number' cannot be passed 'inout' to 'async' function call}}
// expected-error@+1{{var 'number' isolated to global actor 'MyGlobalActor' can not be referenced from this context}}
let _ = Task.runDetached { await { (_ foo: inout Int) async in foo += 1 }(&number) }

// attempt to pass global state owned by the global actor to another async function
// expected-error@+1{{actor-isolated var 'number' cannot be passed 'inout' to 'async' function call}}
@MyGlobalActor func sneaky() async { await modifyAsynchronously(&number) }

// It's okay to pass actor state inout to synchronous functions!

func globalSyncFunction(_ foo: inout Int) { }
@MyGlobalActor func globalActorSyncFunction(_ foo: inout Int) { }
@MyGlobalActor func globalActorAsyncOkay() async { globalActorSyncFunction(&number) }
@MyGlobalActor func globalActorAsyncOkay2() async { globalSyncFunction(&number) }
@MyGlobalActor func globalActorSyncOkay() { globalSyncFunction(&number) }
