// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -I %S/Inputs/custom-modules -enable-experimental-concurrency %s -verify

// REQUIRES: objc_interop
import Foundation
import ObjCConcurrency

func testSlowServer(slowServer: SlowServer) async {
  let _: Int = await slowServer.doSomethingSlow("mail")
  let _: Bool = await slowServer.checkAvailability()
  let _: String = await slowServer.findAnswer() ?? "nope"
  let _: String = await slowServer.findAnswerFailingly() ?? "nope"
  // FIXME: expected-error@-2{{call can throw, but it is not marked with 'try'}}
  // FIXME: expected-error@-2{{call can throw, but it is not marked with 'try'}}
  let _: Void = await slowServer.doSomethingFun("jump")
  let _: (Int) -> Void = slowServer.completionHandler
}

func testSlowServerOldSchool(slowServer: SlowServer) {
  var i1: Int = 0
  slowServer.doSomethingSlow("mail") { i in
    i1 = i
  }
  print(i1)
}
