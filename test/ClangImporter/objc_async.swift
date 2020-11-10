// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules -enable-experimental-concurrency %s -verify

// REQUIRES: objc_interop
// REQUIRES: concurrency
import Foundation
import ObjCConcurrency

func testSlowServer(slowServer: SlowServer) async throws {
  let _: Int = await slowServer.doSomethingSlow("mail")
  let _: Bool = await slowServer.checkAvailability()
  let _: String = try await slowServer.findAnswer()
  let _: String = await try slowServer.findAnswerFailingly()
  let _: Void = await slowServer.doSomethingFun("jump")
  let _: (Int) -> Void = slowServer.completionHandler

  // async version
  let _: Int = await slowServer.doSomethingConflictedAsync("thinking")

  // okay, sync version
  let _: Int = slowServer.doSomethingConflicted("thinking")

  let _: String? = await try slowServer.fortune()
  let _: Int = await try slowServer.magicNumber(withSeed: 42)

  let _: String = slowServer.magicName(withSeed: 42)
  let _: String = await try slowServer.magicNameAsync(withSeed: 42)

  let _: String = try slowServer.boringName(withSeed: 42)
  let _: String = await try slowServer.boringNameAsync(withSeed: 42)

  let _: String = slowServer.confusingName()
  let _: String = await try slowServer.confusingNameAsync()

  let _: String = slowServer.amazingName(5)
  let _: String = await try slowServer.amazingNameAsync()

  let _: String = await try slowServer.terrifyingName()

  await slowServer.serverRestart("localhost")
  await slowServer.server("localhost", atPriorityRestart: 0.8)
}

func testSlowServerSynchronous(slowServer: SlowServer) {
  // synchronous version
  let _: Int = slowServer.doSomethingConflicted("thinking")
}

func testSlowServerOldSchool(slowServer: SlowServer) {
  slowServer.doSomethingSlow("mail") { i in
    _ = i
  }
}
