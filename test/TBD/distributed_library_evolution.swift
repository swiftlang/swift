// REQUIRES: VENDOR=apple
// REQUIRES: concurrency
// REQUIRES: distributed

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/library.swift                                \
// RUN:     -enable-library-evolution                                          \
// RUN:     -disable-availability-checking                                     \
// RUN:     -emit-ir -o %t/test.ll -emit-tbd                                   \
// RUN:     -validate-tbd-against-ir=all                                       \
// RUN:     -emit-tbd-path %t/library.tbd -I %t -tbd-install_name protocol

// RUN: %target-swift-frontend %t/library.swift                                \
// RUN:     -enable-library-evolution                                          \
// RUN:     -disable-availability-checking                                     \
// RUN:     -emit-module                                                       \
// RUN:     -package-name Package                                              \
// RUN:     -module-name Library                                               \
// RUN:     -emit-module-path %t/Library.swiftmodule                           \
// RUN:     -validate-tbd-against-ir=all                                       \
// RUN:     -emit-module-interface-path %t/Library.swiftinterface

// RUN: %target-swift-frontend %t/actor.swift                               \
// RUN:     -enable-library-evolution \
// RUN:     -disable-availability-checking -emit-ir -o %t/test.ll -emit-tbd \
// RUN:     -emit-tbd-path %t/actor.tbd -I %t -tbd-install_name actor

// RUN: %target-swift-frontend %t/actor.swift  \
// RUN:     -I %t                   \
// RUN:     -disable-availability-checking \
// RUN:     -emit-module                                                   \
// RUN:     -package-name Package                                          \
// RUN:     -enable-library-evolution                                      \
// RUN:     -module-name Client                                            \
// RUN:     -emit-module-path %t/Client.swiftmodule                        \
// RUN:     -validate-tbd-against-ir=all                                   \
// RUN:     -emit-module-interface-path %t/Client.swiftinterface


// RUN %llvm-nm -g %t/library.tbd | %FileCheck %s --dump-input=always
// RUN %llvm-nm -g %t/actor.tbd | %FileCheck %s --dump-input=always

//--- library.swift
import Distributed

// CHECK: @"$s4test1AC13_remote_helloyyYaKFTE" = hidden global %swift.async_func_pointer
// CHECK: @"$s4test1AC13_remote_helloyyYaKFTETu" = hidden global %swift.async_func_pointer
public protocol GreeterProtocol: DistributedActor where ActorSystem == LocalTestingDistributedActorSystem {
  distributed func hello(name: String) -> String
}

//--- actor.swift
import Distributed
import Library

public distributed actor SomeDistributedActor: GreeterProtocol {
  public distributed func hello(name: String) -> String {
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
