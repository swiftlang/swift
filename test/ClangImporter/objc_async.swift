// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules %s -verify -verify-additional-file %swift_src_root/test/Inputs/clang-importer-sdk/usr/include/ObjCConcurrency.h -warn-concurrency

// REQUIRES: objc_interop
// REQUIRES: concurrency
import Foundation
import ObjCConcurrency

if #available(SwiftStdlib 5.5, *) {

@MainActor func onlyOnMainActor() { }

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
  // expected-error@-1{{expression is 'async' but is not marked with 'await'}}{{16-16=await }}
  // expected-note@-2{{call is 'async'}}

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

  slowServer.overridableButRunsOnMainThread { s in
    print(s)
    onlyOnMainActor() // okay because parameter is @MainActor
  }

  let _: Int = slowServer.overridableButRunsOnMainThread // expected-error{{cannot convert value of type '(((String) -> Void)?) -> Void' to specified type 'Int'}}
}

func testSlowServerOldSchool(slowServer: SlowServer) {
  slowServer.doSomethingSlow("mail") { i in
    _ = i
  }

  _ = slowServer.allOperations
}

func testSendable(fn: () -> Void) {
  doSomethingConcurrently(fn) // okay, due to implicit @_predatesConcurrency
  doSomethingConcurrentlyButUnsafe(fn) // okay, @Sendable not part of the type

  var x = 17
  doSomethingConcurrently {
    print(x)
    x = x + 1
  }
}

func testSendableInAsync() async {
  var x = 17
  doSomethingConcurrentlyButUnsafe {
    x = 42 // expected-error{{mutation of captured var 'x' in concurrently-executing code}}
  }
  print(x)
}

func testSendableAttrs(
  sendableClass: SendableClass, nonSendableClass: NonSendableClass,
  sendableEnum: SendableEnum, nonSendableEnum: NonSendableEnum,
  sendableOptions: SendableOptions, nonSendableOptions: NonSendableOptions,
  sendableError: SendableError, nonSendableError: NonSendableError,
  sendableStringEnum: SendableStringEnum, nonSendableStringEnum: NonSendableStringEnum,
  sendableStringStruct: SendableStringStruct, nonSendableStringStruct: NonSendableStringStruct
) async {
  func takesSendable<T: Sendable>(_: T) {}

  takesSendable(sendableClass)        // no-error
  takesSendable(nonSendableClass)     // expected-warning{{conformance of 'NonSendableClass' to 'Sendable' is unavailable}}

  doSomethingConcurrently {
    print(sendableClass)               // no-error
    print(nonSendableClass)            // expected-warning{{cannot use parameter 'nonSendableClass' with a non-sendable type 'NonSendableClass' from concurrently-executed code}}

    print(sendableEnum)                // no-error
    print(nonSendableEnum)             // expected-warning{{cannot use parameter 'nonSendableEnum' with a non-sendable type 'NonSendableEnum' from concurrently-executed code}}

    print(sendableOptions)             // no-error
    print(nonSendableOptions)          // expected-warning{{cannot use parameter 'nonSendableOptions' with a non-sendable type 'NonSendableOptions' from concurrently-executed code}}

    print(sendableError)               // no-error
    print(nonSendableError)            // no-error--we don't respect `@_nonSendable` on `ns_error_domain` types because all errors are Sendable

    print(sendableStringEnum)          // no-error
    print(nonSendableStringEnum)       // expected-warning{{cannot use parameter 'nonSendableStringEnum' with a non-sendable type 'NonSendableStringEnum' from concurrently-executed code}}

    print(sendableStringStruct)        // no-error
    print(nonSendableStringStruct)     // expected-warning{{cannot use parameter 'nonSendableStringStruct' with a non-sendable type 'NonSendableStringStruct' from concurrently-executed code}}
  }
}

// Check import of attributes
func globalAsync() async { }

actor MySubclassCheckingSwiftAttributes : ProtocolWithSwiftAttributes {
  func syncMethod() { } // expected-note 2{{calls to instance method 'syncMethod()' from outside of its actor context are implicitly asynchronous}}

  nonisolated func independentMethod() {
    syncMethod() // expected-error{{ctor-isolated instance method 'syncMethod()' can not be referenced from a non-isolated context}}
  }

  nonisolated func nonisolatedMethod() {
  }

  @MainActor func mainActorMethod() {
    syncMethod() // expected-error{{actor-isolated instance method 'syncMethod()' can not be referenced from the main actor}}
  }

  @MainActor func uiActorMethod() { }
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

class MyButton : NXButton {
  @MainActor func testMain() {
    onButtonPress() // okay
  }

  @SomeGlobalActor func testOther() {
    onButtonPress() // expected-error{{call to main actor-isolated instance method 'onButtonPress()' in a synchronous global actor 'SomeGlobalActor'-isolated context}}
  }

  func test() {
    onButtonPress() // okay
  }
}

func testButtons(mb: MyButton) {
  mb.onButtonPress()
}


func testMirrored(instance: ClassWithAsync) async {
  await instance.instanceAsync()
  await instance.protocolMethod()
  await instance.customAsyncName()
}

@MainActor class MyToolbarButton : NXButton {
  var count = 5

  func f() {
    Task {
      let c = count
      print(c)
    }
  }
}

} // SwiftStdlib 5.5
