// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the fake actor systems lib
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/FakeDistributedActorSystems.swiftmodule       \
// RUN:     -module-name FakeDistributedActorSystems                           \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:      %S/../Inputs/FakeDistributedActorSystems.swift                    \
// RUN:     -enable-library-evolution                                          \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -o %t/%target-library-name(FakeDistributedActorSystems)

/// Build the Lib
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/ResilientAPILib.swiftmodule                   \
// RUN:     -module-name ResilientAPILib                                       \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:     %t/src/ResilientAPILib.swift                                       \
// RUN:     -lFakeDistributedActorSystems                                      \
// RUN:     -enable-library-evolution                                          \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -o %t/%target-library-name(ResilientAPILib)

/// Build the ActorLib
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/ResilientImplLib.swiftmodule                  \
// RUN:     -module-name ResilientImplLib                                      \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     %t/src/ResilientImplLib.swift                                      \
// RUN:     -lFakeDistributedActorSystems                                      \
// RUN:     -lResilientAPILib                                                  \
// RUN:     -enable-library-evolution                                          \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -o %t/%target-library-name(ResilientImplLib)

/// Build the client
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library                                                  \
// RUN:     -lFakeDistributedActorSystems                                      \
// RUN:     -lResilientAPILib                                                  \
// RUN:     -lResilientImplLib                                                 \
// RUN:     -module-name main                                                  \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     %s                                                                 \
// RUN:     -enable-library-evolution                                          \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -o %t/a.out

// Sign the main binary and all libraries
// RUN: %target-codesign %t/a.out
// RUN: %target-codesign %t/%target-library-name(FakeDistributedActorSystems)
// RUN: %target-codesign %t/%target-library-name(ResilientAPILib)
// RUN: %target-codesign %t/%target-library-name(ResilientImplLib)

// Run and verify output
// RUN: %env-SWIFT_DUMP_ACCESSIBLE_FUNCTIONS=true %target-run %t/a.out                                                   \
// RUN:     %t/%target-library-name(FakeDistributedActorSystems)               \
// RUN:     %t/%target-library-name(ResilientAPILib)                           \
// RUN:     %t/%target-library-name(ResilientImplLib)                          \
// RUN:     2>&1                                                               \
// RUN:     | %FileCheck %s --color --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// Locating the built libraries failed on Linux (construction of test case),
// but we primarily care about macOS in this test
// UNSUPPORTED: OS=linux-gnu

// %env does not seem to work on Windows
// UNSUPPORTED: OS=windows-msvc

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: remote_run || device_run


// FIXME: Also reproduces the following issue rdar://128284016
//<unknown>:0: error: symbol '$s15ResilientAPILib30ServiceProtocolP8getArray2a12a2SayAA8ResponseVGSaySiG_SSSgtYaKFTj' (dispatch thunk of ResilientAPILib.ServiceProtocol.getArray(a1: Swift.Array<Swift.Int>, a2: Swift.Optional<Swift.String>) async throws -> Swift.Array<ResilientAPILib.Response>) is in generated IR file, but not in TBD file

//<unknown>:0: error: symbol '$s15ResilientAPILib30ServiceProtocolP8getArray2a12a2SayAA8ResponseVGSaySiG_SSSgtYaKFTjTu' (async function pointer to dispatch thunk of ResilientAPILib.ServiceProtocol.getArray(a1: Swift.Array<Swift.Int>, a2: Swift.Optional<Swift.String>) async throws -> Swift.Array<ResilientAPILib.Response>) is in generated IR file, but not in TBD file

//--- ResilientAPILib.swift

import Distributed
import FakeDistributedActorSystems

@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
public struct Response: Codable {}

@Resolvable
@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
public protocol ServiceProtocol: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  distributed func getArray(a1: [Int], a2: String?) -> [Response]
}

//--- ResilientImplLib.swift

import ResilientAPILib

import Distributed
import FakeDistributedActorSystems

@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
public distributed actor ServiceImpl: ServiceProtocol {
  public typealias ActorSystem = FakeRoundtripActorSystem

  public distributed func getArray(a1: [Int], a2: String?) -> [Response] {
    []
  }
}

//--- Main.swift

import ResilientAPILib
import ResilientImplLib

import Distributed
import FakeDistributedActorSystems

@main
struct Main {
  static func main() async throws {
    if #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) {
      let system = FakeRoundtripActorSystem()

      let real: any ServiceProtocol = ServiceImpl(actorSystem: system)

      let proxy: any ServiceProtocol =
        try $ServiceProtocol.resolve(id: real.id, using: system)

      // just in order to see if we crash and trigger the accessible funcs printout
      _ = try await proxy.getArray(a1: [], a2: "")

      // === We expect records for accessible functions from other modules as well,
      // and especially we want to see records for the `$` macro generated stubs,
      // including

      // CHECK: ==== Accessible Function Records ====
      // CHECK: Record name: $s16ResilientImplLib07ServiceB0C8getArray2a12a2Say0A6APILib8ResponseVGSaySiG_SSSgtYaKFTE
      // CHECK: Demangled: distributed thunk ResilientImplLib.ServiceImpl.getArray(a1: Swift.Array<Swift.Int>, a2: Swift.Optional<Swift.String>) async throws -> Swift.Array<ResilientAPILib.Response>

      // CHECK: Record name: $s15ResilientAPILib15ServiceProtocolPAA11Distributed01_E9ActorStubRzrlE8getArray2a12a2SayAA8ResponseVGSaySiG_SSSgtYaKFTE
      // CHECK: Demangled: distributed thunk (extension in ResilientAPILib):ResilientAPILib.ServiceProtocol< where A: Distributed._DistributedActorStub>.getArray(a1: Swift.Array<Swift.Int>, a2: Swift.Optional<Swift.String>) async throws -> Swift.Array<ResilientAPILib.Response>

      // CHECK: Record name: $s15ResilientAPILib16$ServiceProtocolC8getArray2a12a2SayAA8ResponseVGSaySiG_SSSgtYaKFTE
      // CHECK: Demangled: distributed thunk ResilientAPILib.$ServiceProtocol.getArray(a1: Swift.Array<Swift.Int>, a2: Swift.Optional<Swift.String>) async throws -> Swift.Array<ResilientAPILib.Response>

      // CHECK: Record name: $s4main15ServiceProtocolPAA11Distributed01_D9ActorStubRzrlE8getArray2a12a2SayAA8ResponseVGSaySiG_SSSgtYaKFTE
      // CHECK: Demangled: distributed thunk (extension in main):main.ServiceProtocol< where A: Distributed._DistributedActorStub>.getArray(a1: Swift.Array<Swift.Int>, a2: Swift.Optional<Swift.String>) async throws -> Swift.Array<main.Response>

      // CHECK: Record name: $s4main11ServiceImplC8getArray2a12a2SayAA8ResponseVGSaySiG_SSSgtYaKFTE
      // CHECK: Demangled: distributed thunk main.ServiceImpl.getArray(a1: Swift.Array<Swift.Int>, a2: Swift.Optional<Swift.String>) async throws -> Swift.Array<main.Response>

      // CHECK: Record name: $s4main16$ServiceProtocolC8getArray2a12a2SayAA8ResponseVGSaySiG_SSSgtYaKFTE
      // CHECK: Demangled: distributed thunk main.$ServiceProtocol.getArray(a1: Swift.Array<Swift.Int>, a2: Swift.Optional<Swift.String>) async throws -> Swift.Array<main.Response>

      // We expect these to be the exact records we get, no other ones:
      // CHECK: Record count: 6
      
      // CHECK: ==== End of Accessible Function Records ====

      print("DONE") // CHECK: DONE
    }
  }
}
