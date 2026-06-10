// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Distributed/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend %s -O -emit-sil -target %target-swift-5.7-abi-triple -I %t | %FileCheck %s --dump-input=always

// REQUIRES: concurrency
// REQUIRES: distributed

// Verify that dead function elimination does NOT remove distributed thunks,
// distributed method bodies, or their witness table entries. These are invoked
// at runtime via function pointer from the IRGen-generated distributed accessor,
// so they have no direct SIL callers. DFE must treat them as alive.
//
// rdar://168881945

import Distributed
import FakeDistributedActorSystems

protocol MyService: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func distributedMethod() async throws -> String
  func nonDistributedMethod()
}

distributed actor MyServiceImpl: MyService {
  typealias ActorSystem = FakeActorSystem

  distributed func distributedMethod() async throws -> String { "hello" }
  func nonDistributedMethod() {}
}

// The distributed thunk (TE suffix) must survive DFE:
// CHECK: sil hidden [thunk] [distributed] [ref_adhoc_requirement_witness "$s27FakeDistributedActorSystems0A17InvocationDecoderC18decodeNextArgumentxyKSeRzSERzlF"] @$s37dead_function_elimination_distributed13MyServiceImplC0D6MethodSSyYaKFTE : $@convention(method) @async (@guaranteed MyServiceImpl) -> (@owned String, @error any Error) {

// The distributed method body must survive DFE:
// CHECK: sil hidden [distributed] @$s37dead_function_elimination_distributed13MyServiceImplC0D6MethodSSyYaKF : $@convention(method) @async (@sil_isolated @guaranteed MyServiceImpl) -> (@owned String, @error any Error) {

// The witness table must still contain entries for the distributed func and thunk:
// CHECK-LABEL: sil_witness_table hidden MyServiceImpl: MyService module dead_function_elimination_distributed {
// protocol witness (TW):
// CHECK:   method #MyService.distributedMethod!distributed({{.*}}): <Self where Self : MyService> (isolated Self) -> () async throws -> String : @$s37dead_function_elimination_distributed13MyServiceImplCAA0eF0A2aDP0D6MethodSSyYaKFTW
// protocol thunk witness (TWTE):
// CHECK:   method #MyService.distributedMethod!distributed_thunk({{.*}}): <Self where Self : MyService> (Self) -> () async throws -> String : @$s37dead_function_elimination_distributed13MyServiceImplCAA0eF0A2aDP0D6MethodSSyYaKFTWTE

// The not used not-distributed method witness was dead eliminated:
// CHECK:   method #MyService.nonDistributedMethod: <Self where Self : MyService> (isolated Self) -> () -> () : nil
