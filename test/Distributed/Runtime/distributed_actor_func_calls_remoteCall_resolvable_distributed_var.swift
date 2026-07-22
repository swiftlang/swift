// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -plugin-path %swift-plugin-dir -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -emit-silgen -module-name main -plugin-path %swift-plugin-dir -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -plugin-path %swift-plugin-dir -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

@Resolvable
@available(SwiftStdlib 6.0, *)
protocol Service: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  distributed func myCoolMethodCallMe() -> String
  distributed var exampleVariable: String { get }
}

@available(SwiftStdlib 6.0, *)
distributed actor ServiceImpl: Service {
  distributed func myCoolMethodCallMe() -> String { "hello" }
  distributed var exampleVariable: String { "hello" }
}

@available(SwiftStdlib 6.0, *)
func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = ServiceImpl(actorSystem: system)
  let ref = try ServiceImpl.resolve(id: local.id, using: system)

  // let hello = try await ref.myCoolMethodCallMe()
  // bb1(%23 : @owned $$Service):                     // Preds: bb0
  //  end_borrow %20                                  // id: %24
  //  destroy_value %19                               // id: %25
  //  end_borrow %17                                  // id: %26
  //  %27 = move_value [lexical] [var_decl] %23       // users: %62, %77, %29, %28
  //  debug_value %27, let, name "ref"                // id: %28
  //  %29 = begin_borrow %27                          // users: %34, %32, %76, %31
  //  // function_ref Service<>.myCoolMethodCallMe()
  //  %30 = function_ref @$s4main7ServicePAA11Distributed01_C9ActorStubRzrlE18myCoolMethodCallMeSSyYaKFTE : $@convention(method) @async <τ_0_0 where τ_0_0 : _DistributedActorStub, τ_0_0 : Service> (@guaranteed τ_0_0) -> (@owned String, @error any Error) // user: %32
  //  hop_to_executor %29                             // id: %31
  //  try_apply %30<$Service>(%29) : $@convention(method) @async <τ_0_0 where τ_0_0 : _DistributedActorStub, τ_0_0 : Service> (@guaranteed τ_0_0) -> (@owned String, @error any Error), normal bb2, error bb4 // id: %32

  let hello = try await ref.exampleVariable
  // bb1(%23 : @owned $$Service):                     // Preds: bb0
  //  end_borrow %20                                  // id: %24
  //  destroy_value %19                               // id: %25
  //  end_borrow %17                                  // id: %26
  //  %27 = move_value [lexical] [var_decl] %23       // users: %61, %76, %29, %28
  //  debug_value %27, let, name "ref"                // id: %28
  //  %29 = begin_borrow %27                          // users: %36, %31, %75, %30
  //  hop_to_executor %29                             // id: %30
  //  try_apply undef<$Service>(%29) : $@convention(method) @async <τ_0_0 where τ_0_0 : _DistributedActorStub, τ_0_0 : Service> (@sil_isolated @guaranteed τ_0_0) -> (@owned String, @error any Error), normal bb2, error bb4 // id: %31

  // Compared to a variable on the concrete ServiceImpl:
  //      // %23                                            // user: %27
  //      bb1(%23 : @owned $ServiceImpl):                   // Preds: bb0
  //      end_borrow %20                                  // id: %24
  //      destroy_value %19                               // id: %25
  //      end_borrow %17                                  // id: %26
  //      %27 = move_value [lexical] [var_decl] %23       // users: %62, %77, %29, %28
  //      debug_value %27, let, name "ref"                // id: %28
  //      %29 = begin_borrow %27                          // users: %37, %32, %76, %31
  //      // function_ref ServiceImpl.exampleVariable()
  //      %30 = function_ref @$s4main11ServiceImplC15exampleVariableSSyYaKFTE : $@convention(method) @async (@guaranteed ServiceImpl) -> (@owned String, @error any Error) // user: %32
  //      hop_to_executor %29                             // id: %31
  //      try_apply %30(%29) : $@convention(method) @async (@guaranteed ServiceImpl) -> (@owned String, @error any Error), normal bb2, error bb4 // id: %32

  // error: function type mismatch, declared as
  //    @convention(method) @async <τ_0_0 where τ_0_0 : _DistributedActorStub, τ_0_0 : Service> (@guaranteed τ_0_0) -> (@owned String, @error any Error)
  // but used as
  //    @convention(method) @async <τ_0_0 where τ_0_0 : _DistributedActorStub, τ_0_0 : Service> (@sil_isolated @guaranteed τ_0_0) -> (@owned String, @error any Error)
  precondition(hello == "hello")
}

@available(SwiftStdlib 6.0, *)
@main struct Main {
  static func main() async {
    try! await test()

    print("Done")
    // CHECK: Done
  }
}
