// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules -enable-experimental-concurrency %s -verify

// REQUIRES: objc_interop
// REQUIRES: concurrency
import Foundation
import ObjCConcurrency

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
}

func testSlowServerSynchronous(slowServer: SlowServer) {
  // synchronous version
  let _: Int = slowServer.doSomethingConflicted("thinking")
  slowServer.poorlyNamed("hello") { (i: Int) in print(i) }
  slowServer.customize(with: "hello") { (i: Int) in print(i) }

  slowServer.dance("jig") { s in print(s + "") }
  slowServer.leap(17) { s in print(s + "") }
  slowServer.repeatTrick("jump") { i in print(i + 1) }
}

func testSlowServerOldSchool(slowServer: SlowServer) {
  slowServer.doSomethingSlow("mail") { i in
    _ = i
  }

  _ = slowServer.allOperations
}

// Check import of attributes
func globalAsync() async { }

actor MySubclassCheckingSwiftAttributes : ProtocolWithSwiftAttributes {
  func syncMethod() { } // expected-note 2{{calls to instance method 'syncMethod()' from outside of its actor context are implicitly asynchronous}}

  func independentMethod() {
    syncMethod() // expected-error{{ctor-isolated instance method 'syncMethod()' can not be referenced from an '@actorIndependent' context}}
  }

  func asyncHandlerMethod() {
    await globalAsync() // okay because we infer @asyncHandler from the protocol
  }

  func mainActorMethod() {
    syncMethod() // expected-error{{actor-isolated instance method 'syncMethod()' can not be referenced from context of global actor 'MainActor'}}
  }
}
