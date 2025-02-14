// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library -plugin-path %swift-plugin-dir -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
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

struct SuperLargeSizeStruct: Codable {
  let i1a: Int = 0
  let i2a: Int = 0
  let i3a: Int = 0
  let i4a: Int = 0
  let i5a: Int = 0
  let i6a: Int = 0
  let i7a: Int = 0
  let i8a: Int = 0
}

@available(SwiftStdlib 6.0, *)
distributed actor TheWorker {
  typealias ActorSystem = DefaultDistributedActorSystem

  distributed func callMeCallMe() async throws -> SuperLargeSizeStruct {
    return .init()
  }
}

@available(SwiftStdlib 6.0, *)
func test_generic(system: DefaultDistributedActorSystem) async throws {
  let localW = TheWorker(actorSystem: system)
  let remoteW = try! TheWorker.resolve(id: localW.id, using: system)

  let target = RemoteCallTarget("$s4main9TheWorkerC010callMeCallE0AA20SuperLargeSizeStructVyYaKFTE")
  var invocation = FakeInvocationEncoder()
  try invocation.recordReturnType(SuperLargeSizeStruct.self)
  try invocation.recordErrorType(Error.self)
  try invocation.doneRecording()

  do {
    try await system.remoteCall(
      on: localW,
      target: target,
      invocation: &invocation,
      throwing: Error.self,
      returning: SuperLargeSizeStruct.self
    )
    // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.callMeCallMe(), invocation:FakeInvocationEncoder(genericSubs: [], arguments: [], returnType: Optional(main.SuperLargeSizeStruct), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:main.SuperLargeSizeStruct
    // CHECK: > execute distributed target: main.TheWorker.callMeCallMe(), identifier: $s4main9TheWorkerC010callMeCallE0AA20SuperLargeSizeStructVyYaKFTE
    // CHECK: << remoteCall return: SuperLargeSizeStruct
  }
  print("==== ----------------------------------------------------------------")
}

@available(SwiftStdlib 6.0, *)
@main struct Main {
  static func main() async {
    let system = DefaultDistributedActorSystem()
    print("===================================================================")
    try! await test_generic(system: system)
  }
}
