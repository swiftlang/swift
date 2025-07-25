// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the fake actor systems lib
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/FakeDistributedActorSystems.swiftmodule       \
// RUN:     -module-name FakeDistributedActorSystems                           \
// RUN:      %S/../Inputs/FakeDistributedActorSystems.swift                    \
// RUN:     -enable-library-evolution                                          \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -o %t/%target-library-name(FakeDistributedActorSystems)

/// Build the ResilientAPILib
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -emit-module-path %t/ResilientAPILib.swiftmodule                   \
// RUN:     -module-name ResilientAPILib                                       \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:     %t/src/ResilientAPILib.swift                                       \
// RUN:     %t/src/ResilientAPILibFile2.swift                                  \
// RUN:     -lFakeDistributedActorSystems                                      \
// RUN:     -enable-library-evolution                                          \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -o %t/%target-library-name(ResilientAPILib)

/// Build the ResilientImplLib
// RUN: %target-build-swift                                                    \
// RUN:     -target %target-swift-6.0-abi-triple                               \
// RUN:     -parse-as-library -emit-library                                    \
// RUN:     -module-name ResilientImplLib                                      \
// RUN:     -I %t                                                              \
// RUN:     -L %t                                                              \
// RUN:     -plugin-path %swift-plugin-dir                                     \
// RUN:     %t/src/ResilientImplLib.swift                                      \
// RUN:     -lFakeDistributedActorSystems                                      \
// RUN:     -lResilientAPILib                                                  \
// RUN:     -enable-library-evolution                                          \
// RUN:     -Xfrontend -validate-tbd-against-ir=all                            \
// RUN:     -emit-irgen                                                        \
// RUN:     | %FileCheck %s --color --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// Locating the built libraries failed on Linux (construction of test case),
// but we primarily care about macOS in this test
// UNSUPPORTED: OS=linux-gnu || OS=freebsd

// %env does not seem to work on Windows
// UNSUPPORTED: OS=windows-msvc

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: remote_run || device_run

//--- ResilientAPILib.swift

import Distributed
import FakeDistributedActorSystems

@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
public struct Response: Codable {}

@Resolvable
@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
public protocol DistributedNotificationService: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  distributed func getArray(a1: [Int], a2: String?) -> [Response]
}

public protocol IdentifiableActor {
    static var actorID: FakeRoundtripActorSystem.ActorID { get }
}

//--- ResilientAPILibFile2.swift

final class Another { }

//--- ResilientImplLib.swift

import ResilientAPILib

import Distributed
import FakeDistributedActorSystems

@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
public distributed actor ServiceImpl: DistributedNotificationService, IdentifiableActor {
  public typealias ActorSystem = FakeRoundtripActorSystem

  public static var actorID: FakeRoundtripActorSystem.ActorID {
    .init(parse: "test")
  }

  public distributed func getArray(a1: [Int], a2: String?) -> [Response] {
    []
  }
}

extension ServiceImpl {}

//--- ResilientImplLibFile2.swift

class AnotherImpl {}


//        @"protocol conformance descriptor for ResilientImplLib.ServiceImpl : ResilientAPILib.DistributedNotificationService in ResilientImplLib" = constant {
// CHECK: @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AAMc" = constant {
// CHECK-SAME:   i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32
// CHECK-SAME: } {
// CHECK-SAME:   i32 add (
// CHECK-SAME:     i32 trunc (
// CHECK-SAME:       i64 sub (
//                     i64 ptrtoint (ptr @"got.protocol descriptor for ResilientAPILib.DistributedNotificationService" to i64),
// CHECK-SAME:         i64 ptrtoint (ptr @"got.$s15ResilientAPILib30DistributedNotificationServiceMp" to i64),
//                     i64 ptrtoint (ptr @"protocol conformance descriptor for ResilientImplLib.ServiceImpl : ResilientAPILib.DistributedNotificationService in ResilientImplLib" to i64)
// CHECK-SAME:         i64 ptrtoint (ptr @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AAMc" to i64)
// CHECK-SAME:       ) to i32
// CHECK-SAME:     ),
// CHECK-SAME:     i32 1
// CHECK-SAME:   ),
//
// CHECK-SAME:   i32 trunc (
// CHECK-SAME:     i64 sub (
//                   i64 ptrtoint (ptr @"nominal type descriptor for ResilientImplLib.ServiceImpl" to i64),
// CHECK-SAME:       i64 ptrtoint (ptr @"$s16ResilientImplLib07ServiceB0CMn" to i64),
// CHECK-SAME:       i64 ptrtoint (ptr getelementptr inbounds (
// CHECK-SAME:         { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 },
// CHECK-SAME:         ptr @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AAMc",
// CHECK-SAME:         i32 0, i32 1
// CHECK-SAME:       ) to i64)
// CHECK-SAME:     ) to i32
// CHECK-SAME:   ),
//
// CHECK-SAME:   i32 0,
// CHECK-SAME:   i32 196608,
// CHECK-SAME:   i32 3,
//
// CHECK-SAME:   i32 add (
// CHECK-SAME:     i32 trunc (
// CHECK-SAME:       i64 sub (
//                     i64 ptrtoint (ptr @"got.base conformance descriptor for ResilientAPILib.DistributedNotificationService: Distributed.DistributedActor" to i64),
// CHECK-SAME:         i64 ptrtoint (ptr @"got.$s15ResilientAPILib30DistributedNotificationServiceP0C00C5ActorTb" to i64),
// CHECK-SAME:         i64 ptrtoint (ptr getelementptr inbounds (
// CHECK-SAME:           { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 },
// CHECK-SAME:           ptr @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AAMc",
// CHECK-SAME:           i32 0, i32 5
// CHECK-SAME:         ) to i64)
// CHECK-SAME:       ) to i32
// CHECK-SAME:     ),
// CHECK-SAME:     i32 1
// CHECK-SAME:   ),
//
// CHECK-SAME:   i32 trunc (
// CHECK-SAME:     i64 sub (
// CHECK-SAME:       i64 ptrtoint (ptr getelementptr (
// CHECK-SAME:         i8, ptr @"associated conformance 16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AA0F00F5Actor",
// CHECK-SAME:         i64 1
// CHECK-SAME:       ) to i64),
// CHECK-SAME:       i64 ptrtoint (ptr getelementptr inbounds (
// CHECK-SAME:         { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 },
// CHECK-SAME:         ptr @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AAMc",
// CHECK-SAME:         i32 0, i32 6
// CHECK-SAME:       ) to i64)
// CHECK-SAME:     ) to i32
// CHECK-SAME:   ),
//
// CHECK-SAME:   i32 add (
// CHECK-SAME:     i32 trunc (
// CHECK-SAME:       i64 sub (
//                     i64 ptrtoint (ptr @"got.method descriptor for ResilientAPILib.DistributedNotificationService.getArray(a1: [Swift.Int], a2: Swift.String?) -> [ResilientAPILib.Response]" to i64),
// CHECK-SAME:         i64 ptrtoint (ptr @"got.$s15ResilientAPILib30DistributedNotificationServiceP8getArray2a12a2SayAA8ResponseVGSaySiG_SSSgtFTq" to i64),
// CHECK-SAME:         i64 ptrtoint (ptr getelementptr inbounds (
// CHECK-SAME:           { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 },
// CHECK-SAME:           ptr @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AAMc",
// CHECK-SAME:           i32 0, i32 7
// CHECK-SAME:         ) to i64)
// CHECK-SAME:       ) to i32
// CHECK-SAME:     ),
// CHECK-SAME:     i32 1
// CHECK-SAME:   ),
//
// CHECK-SAME:   i32 trunc (
// CHECK-SAME:     i64 sub (
//                   i64 ptrtoint (ptr @"protocol witness for ResilientAPILib.DistributedNotificationService.getArray(a1: [Swift.Int], a2: Swift.String?) -> [ResilientAPILib.Response] in conformance ResilientImplLib.ServiceImpl : ResilientAPILib.DistributedNotificationService in ResilientImplLib" to i64),
// CHECK-SAME:       i64 ptrtoint (ptr @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AadEP8getArray2a12a2SayAD8ResponseVGSaySiG_SSSgtFTW" to i64),
// CHECK-SAME:       i64 ptrtoint (ptr getelementptr inbounds (
// CHECK-SAME:         { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 },
//                     ptr @"protocol conformance descriptor for ResilientImplLib.ServiceImpl : ResilientAPILib.DistributedNotificationService in ResilientImplLib",
// CHECK-SAME:         ptr @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AAMc",
// CHECK-SAME:         i32 0, i32 8
// CHECK-SAME:       ) to i64)
// CHECK-SAME:     ) to i32
// CHECK-SAME:   ),
//
// THIS ONE HAS .7 IN OUR REAL EXAMPLE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
// CHECK-SAME:   i32 add (
// CHECK-SAME:     i32 trunc (
// CHECK-SAME:       i64 sub (
//                     i64 ptrtoint (ptr @"got.method descriptor for ResilientAPILib.DistributedNotificationService.getArray(a1: [Swift.Int], a2: Swift.String?) async throws -> [ResilientAPILib.Response]" to i64),
// CHECK-SAME:         i64 ptrtoint (ptr @"got.$s15ResilientAPILib30DistributedNotificationServiceP8getArray2a12a2SayAA8ResponseVGSaySiG_SSSgtYaKFTqTE" to i64),
// CHECK-SAME:         i64 ptrtoint (ptr getelementptr inbounds (
// CHECK-SAME:           { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 },
//                       ptr @"protocol conformance descriptor for ResilientImplLib.ServiceImpl : ResilientAPILib.DistributedNotificationService in ResilientImplLib",
// CHECK-SAME:           ptr @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AAMc",
// CHECK-SAME:           i32 0, i32 9
// CHECK-SAME:         ) to i64)
// CHECK-SAME:       ) to i32
// CHECK-SAME:     ),
// CHECK-SAME:     i32 1
// CHECK-SAME:   ),
// <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
//
// CHECK-SAME:   i32 trunc (
// CHECK-SAME:     i64 sub (
//                   i64 ptrtoint (ptr @"async function pointer to distributed thunk protocol witness for ResilientAPILib.DistributedNotificationService.getArray(a1: [Swift.Int], a2: Swift.String?) async throws -> [ResilientAPILib.Response] in conformance ResilientImplLib.ServiceImpl : ResilientAPILib.DistributedNotificationService in ResilientImplLib" to i64),
// CHECK-SAME:       i64 ptrtoint (ptr @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AadEP8getArray2a12a2SayAD8ResponseVGSaySiG_SSSgtYaKFTWTETu" to i64),
// CHECK-SAME:       i64 ptrtoint (ptr getelementptr inbounds (
// CHECK-SAME:         { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 },
//                     ptr @"protocol conformance descriptor for ResilientImplLib.ServiceImpl : ResilientAPILib.DistributedNotificationService in ResilientImplLib",
// CHECK-SAME:         ptr @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AAMc",
// CHECK-SAME:         i32 0, i32 10
// CHECK-SAME:       ) to i64)
// CHECK-SAME:     ) to i32
// CHECK-SAME:   ),
//
// CHECK-SAME:   i16 0,
// CHECK-SAME:   i16 1,
// CHECK-SAME:   i32 0,
//
// CHECK-SAME:   i32 trunc (
// CHECK-SAME:     i64 sub (
//                   i64 ptrtoint (ptr @"metadata instantiation cache for protocol conformance descriptor for ResilientImplLib.ServiceImpl : ResilientAPILib.DistributedNotificationService in ResilientImplLib" to i64),
// CHECK-SAME:       i64 ptrtoint (ptr @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AAMcMK" to i64),
// CHECK-SAME:       i64 ptrtoint (ptr getelementptr inbounds (
// CHECK-SAME:         { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 },
//                     ptr @"protocol conformance descriptor for ResilientImplLib.ServiceImpl : ResilientAPILib.DistributedNotificationService in ResilientImplLib",
// CHECK-SAME:         ptr @"$s16ResilientImplLib07ServiceB0C0A6APILib023DistributedNotificationD0AAMc",
// CHECK-SAME:         i32 0, i32 14
// CHECK-SAME:       ) to i64)
// CHECK-SAME:     ) to i32
// CHECK-SAME:   )
// CHECK-SAME: }, section "__TEXT,__const", no_sanitize_address, align 4
