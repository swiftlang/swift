// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the fake actor systems lib
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-5.7-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/FakeDistributedActorSystems.swiftmodule       \
// RUN:     -module-name FakeDistributedActorSystems                           \
// RUN:      %S/../Inputs/FakeDistributedActorSystems.swift                    \
// RUN:     -enable-library-evolution                                          \
// RUN:     -o %t/%target-library-name(FakeDistributedActorSystems)

/// Build the Lib
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-5.7-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/ResilientLib.swiftmodule                      \
// RUN:     -module-name ResilientLib                                          \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     %t/src/ResilientLib.swift                                          \
// RUN:     -enable-library-evolution                                          \
// RUN:     -o %t/%target-library-name(ResilientLib)

/// Build the ActorLib
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-5.7-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/ResilientActorLib.swiftmodule                 \
// RUN:     -module-name ResilientActorLib                                     \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     %t/src/ResilientActorLib.swift                                     \
// RUN:     -lFakeDistributedActorSystems                                      \
// RUN:     -lResilientLib                                                     \
// RUN:     -enable-library-evolution                                          \
// RUN:     -o %t/%target-library-name(ResilientActorLib)

/// Build the client
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-5.7-abi-triple                               \
// RUN:     -parse-as-library                                                  \
// RUN:     -lFakeDistributedActorSystems                                      \
// RUN:     -lResilientLib                                                     \
// RUN:     -lResilientActorLib                                                \
// RUN:     -module-name main                                                  \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     %s                                                                 \
// RUN:     -enable-library-evolution                                          \
// RUN:     -o %t/a.out

// Sign the main binary and all libraries
// RUN: %target-codesign %t/a.out
// RUN: %target-codesign %t/%target-library-name(FakeDistributedActorSystems)
// RUN: %target-codesign %t/%target-library-name(ResilientActorLib)
// RUN: %target-codesign %t/%target-library-name(ResilientLib)

// Run and verify output
// RUN: %target-run %t/a.out                                                   \
// RUN:     %t/%target-library-name(FakeDistributedActorSystems)               \
// RUN:     %t/%target-library-name(ResilientActorLib)                         \
// RUN:     %t/%target-library-name(ResilientLib)                              \
// RUN:     | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// Locating the built libraries failed on Linux (construction of test case),
// but we primarily care about macOS in this test
// UNSUPPORTED: OS=linux-gnu
// UNSUPPORTED: OS=freebsd

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: remote_run || device_run

//--- ResilientLib.swift

import Distributed

public protocol SomeProtocol {
  func function() async throws -> String
}

//--- ResilientActorLib.swift

import ResilientLib

import Distributed
import FakeDistributedActorSystems

public distributed actor Impl: SomeProtocol {
  public typealias ActorSystem = FakeRoundtripActorSystem

  public distributed func function() async throws -> String {
    "Success!"
  }
}

//--- Main.swift

import ResilientLib
import ResilientActorLib

import Distributed
import FakeDistributedActorSystems

@main struct Main {
  static func main() async {
    let system = FakeRoundtripActorSystem()

    print("start")

    let impl = Impl(actorSystem: system)

    let anyAct: any SomeProtocol = impl
    let anyReply = try! await anyAct.function()
    print("any reply = \(anyReply)") // CHECK: any reply = Success!

    let proxy: any SomeProtocol = try! Impl.resolve(id: impl.id, using: system)
    let proxyReply = try! await proxy.function()
    print("proxy reply = \(proxyReply)") // CHECK: proxy reply = Success!

    print("done")
  }
}
