// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main  -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s 

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor Greeter {
  distributed func usedToHaveAsyncBytNotAnymore()
    /*previously had async, and remote called invoked such async method */ -> String {
    return #function
  }
}

distributed actor Helloer {
  distributed func usedToBeSyncButNowIsAsync()
    /* The remote caller thinks this method was not async, but actually it is */
    async -> String {
    return #function
  }
}

func test_usedToBeAsync_but_remoteImplIsNotAnymore() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Greeter(actorSystem: system)
  let remote = try Greeter.resolve(id: local.id, using: system)

  var invocation = FakeInvocationEncoder()
  // let reply = try await remote.usedToHaveAsyncBytNotAnymore()
  let reply: String = try await system.remoteCall(
    on: remote,
    // Important: notice the mangling has a YaK here, since we mangled
    // based on the distributed thunk, which is 'async throws' (YaK).
    //
    // This test is about ensuring, that even if a remote recipient process,
    // changed their implementation from async to not async, we're still able
    // to invoke them. From the perspective of the remote caller it truly does
    // not matter how the recipient implements this call: is it async or not,
    // and we should not tie ability to invoke a method to it's asyncness.
    //
    // Note also that a remote call is always async and throws as well,
    // so mangling based on the 'async throws' thunk does not introduce
    // unexpected effects in remote calls.
    //
    // Design limitation by choice: this means we cannot overload distributed methods on async-ness alone
    target: RemoteCallTarget("$s4main7GreeterC28usedToHaveAsyncBytNotAnymoreSSyYaKFTE"),
    invocation: &invocation,
    throwing: Never.self,
    returning: String.self
  )

  // CHECK: >> remoteCall: on:main.Greeter, target:main.Greeter.usedToHaveAsyncBytNotAnymore(), invocation:FakeInvocationEncoder(genericSubs: [], arguments: [], returnType: nil, errorType: nil), throwing:Swift.Never, returning:Swift.String
  // CHECK: << onReturn: usedToHaveAsyncBytNotAnymore()

  print("reply: \(reply)") // CHECK: reply: usedToHaveAsyncBytNotAnymore()
}

func test_usedToBeSync_but_remoteImplIsAsyncNow() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Helloer(actorSystem: system)
  let remote = try Helloer.resolve(id: local.id, using: system)

  var invocation = FakeInvocationEncoder()
  //  let reply: String = try await remote.usedToBeSyncButNowIsAsync()
  let reply: String = try await system.remoteCall(
    on: remote,
    target: RemoteCallTarget("$s4main7HelloerC25usedToBeSyncButNowIsAsyncSSyYaKFTE"),
    invocation: &invocation,
    throwing: Never.self,
    returning: String.self
  )

  // CHECK: >> remoteCall: on:main.Helloer, target:main.Helloer.usedToBeSyncButNowIsAsync(), invocation:FakeInvocationEncoder(genericSubs: [], arguments: [], returnType: nil, errorType: nil), throwing:Swift.Never, returning:Swift.String
  // CHECK: << onReturn: usedToBeSyncButNowIsAsync()

  print("reply: \(reply)") // CHECK: reply: usedToBeSyncButNowIsAsync()
}

@main struct Main {
  static func main() async {
    try! await test_usedToBeAsync_but_remoteImplIsNotAnymore()
    try! await test_usedToBeSync_but_remoteImplIsAsyncNow()
  }
}
