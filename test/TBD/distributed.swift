// REQUIRES: VENDOR=apple
// REQUIRES: concurrency
// REQUIRES: distributed

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-testing -enable-experimental-distributed -disable-availability-checking -emit-ir -o %t/test.ll -emit-tbd -emit-tbd-path %t/test.tbd
// RUN cat %t/test.tbd | %FileCheck %s --dump-input=always

import _Distributed

// CHECK: @"$s4test1AC13_remote_helloyyYaKFTE" = hidden global %swift.async_func_pointer
// CHECK: @"$s4test1AC13_remote_helloyyYaKFTETu" = hidden global %swift.async_func_pointer
distributed actor SomeDistributedActor {
  distributed func hello(name: String) -> String {
    "Hello, \(name)!"
  }
}

// function:
// IR unmangledName = $s4test20SomeDistributedActorC5hello4nameS2S_tF
// function method descriptor
// IR unmangledName = $s4test20SomeDistributedActorC5hello4nameS2S_tFTq
// thunk, method reference
// IR unmangledName = $s4test20SomeDistributedActorC5hello4nameS2S_tFTE
// thunk, method reference + async function pointer
// IR unmangledName = $s4test20SomeDistributedActorC5hello4nameS2S_tFTETu