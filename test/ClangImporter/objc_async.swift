// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules -enable-experimental-concurrency -enable-experimental-async-handler %s -verify

// REQUIRES: objc_interop
// REQUIRES: concurrency
import Foundation
import ObjCConcurrency

@MainActor func onlyOnMainActor() { } // expected-note{{calls to global function 'onlyOnMainActor()' from outside of its actor context are implicitly asynchronous}}

func testSlowServer(slowServer: SlowServer) async throws {
  let _: Int = await slowServer.doSomethingSlow("mail")
  let _: Bool = await slowServer.checkAvailability()
  let _: String = try await slowServer.findAnswer()
  let _: String = try await slowServer.findAnswerFailingly()

  let (aOpt, b) = try await slowServer.findQAndA()
  if let a = aOpt { // make sure aOpt is optional
    print(a)
  }
  let _: String = b // make sure b is non-optional

  let _: String = try await slowServer.findAnswer()

  let _: Void = await slowServer.doSomethingFun("jump")
  let _: (Int) -> Void = slowServer.completionHandler

  // async version
  let _: Int = await slowServer.doSomethingConflicted("thinking")

  // still async version...
  let _: Int = slowServer.doSomethingConflicted("thinking")
  // expected-error@-1{{call is 'async' but is not marked with 'await'}}{{16-16=await }}

  let _: String? = try await slowServer.fortune()
  let _: Int = try await slowServer.magicNumber(withSeed: 42)

  await slowServer.serverRestart("localhost")
  await slowServer.serverRestart("localhost", atPriority: 0.8)

  _ = await slowServer.allOperations()

  let _: Int = await slowServer.bestName("hello")
  let _: Int = await slowServer.customize("hello")

  let _: String = await slowServer.dance("slide")
  let _: String = await slowServer.__leap(17)

  slowServer.repeatTrick("jump") // expected-error{{missing argument for parameter 'completionHandler' in call}}

  _ = try await slowServer.someAsyncMethod()


  _ = await slowServer.operations()

  _ = await slowServer.runOnMainThread()
}

func testSlowServerSynchronous(slowServer: SlowServer) {
  // synchronous version
  let _: Int = slowServer.doSomethingConflicted("thinking")
  slowServer.poorlyNamed("hello") { (i: Int) in print(i) }
  slowServer.customize(with: "hello") { (i: Int) in print(i) }

  slowServer.dance("jig") { s in print(s + "") }
  slowServer.leap(17) { s in print(s + "") }
  slowServer.repeatTrick("jump") { i in print(i + 1) }

  let s = slowServer.operations
  _ = s + []

  slowServer.runOnMainThread { s in
    print(s)
    onlyOnMainActor() // okay because runOnMainThread has a @MainActor closure
  }
}

func testSlowServerOldSchool(slowServer: SlowServer) {
  slowServer.doSomethingSlow("mail") { i in
    _ = i
  }

  _ = slowServer.allOperations
}

func testSendable(fn: () -> Void) { // expected-note{{parameter 'fn' is implicitly non-concurrent}}
  doSomethingConcurrently(fn)
  // expected-error@-1{{passing non-concurrent parameter 'fn' to function expecting a @Sendable closure}}

  var x = 17
  doSomethingConcurrently {
    print(x) // expected-error{{reference to captured var 'x' in concurrently-executing code}}
    x = x + 1 // expected-error{{mutation of captured var 'x' in concurrently-executing code}}
    // expected-error@-1{{reference to captured var 'x' in concurrently-executing code}}
  }
}

// Check import of attributes
func globalAsync() async { }

actor MySubclassCheckingSwiftAttributes : ProtocolWithSwiftAttributes {
  func syncMethod() { } // expected-note 2{{calls to instance method 'syncMethod()' from outside of its actor context are implicitly asynchronous}}

  func independentMethod() {
    syncMethod() // expected-error{{ctor-isolated instance method 'syncMethod()' can not be referenced from a non-isolated context}}
  }

  func asyncHandlerMethod() {
    await globalAsync() // okay because we infer @asyncHandler from the protocol
  }

  func mainActorMethod() {
    syncMethod() // expected-error{{actor-isolated instance method 'syncMethod()' can not be referenced from synchronous context of global actor 'MainActor'}}
  }

  func uiActorMethod() { }
}

// Sendable conformance inference for imported types.
func acceptCV<T: Sendable>(_: T) { }
func testCV(r: NSRange) {
  acceptCV(r)
}

// Global actor (unsafe) isolation.

actor SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

@SomeGlobalActor(unsafe) func unsafelyOnSomeGlobal() { }

class MyButton : NXButton {
  @MainActor func testMain() {
    onButtonPress() // okay
  }

  @SomeGlobalActor func testOther() {
    onButtonPress() // expected-error{{instance method 'onButtonPress()' isolated to global actor 'MainActor' can not be referenced from different global actor 'SomeGlobalActor'}}
  }

  func test() { // expected-note{{add '@MainActor' to make instance method 'test()' part of global actor 'MainActor'}}
    onButtonPress() // okay, onButtonPress is @MainActor(unsafe)
    unsafelyOnSomeGlobal() // okay, we haven't opted into anything
    onlyOnMainActor() // expected-error{{global function 'onlyOnMainActor()' isolated to global actor 'MainActor' can not be referenced from this synchronous context}}
  }
}

class MyOtherButton: NXButton {
  override func onButtonPress() { // expected-note{{calls to instance method 'onButtonPress()' from outside of its actor context are implicitly asynchronous}}
    onlyOnMainActor() // yes, we're on the main actor
    unsafelyOnSomeGlobal() // okay, we haven't opted into any actual checking
  }

  func test() {
    onButtonPress() // okay, it's @MainActor(unsafe)
  }

  @SomeGlobalActor func testOther() {
    onButtonPress() // expected-error{{instance method 'onButtonPress()' isolated to global actor 'MainActor' can not be referenced from different global actor 'SomeGlobalActor' in a synchronous context}}
  }
}


func testButtons(mb: MyButton) {
  mb.onButtonPress()
}
