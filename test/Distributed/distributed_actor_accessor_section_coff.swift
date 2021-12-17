// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -emit-irgen -module-name distributed_actor_accessors -enable-experimental-distributed -disable-availability-checking -I %t 2>&1 %s | %IRGenFileCheck %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

// REQUIRES: OS=windows-msvc
// FIXME: Test is temporary disabled (no way to debug)
// REQUIRES: fix

import _Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

enum SimpleE : Codable {
case a
}

enum E : Codable {
case a, b, c
}

enum IndirectE : Codable {
  case empty
  indirect case test(_: Int)
}

final class Obj : Codable, Sendable {
  let x: Int

  init(x: Int) {
    self.x = x
  }
}

struct LargeStruct : Codable {
  var a: Int
  var b: Int
  var c: String
  var d: Double
}

@available(SwiftStdlib 5.6, *)
public distributed actor MyActor {
  distributed func simple1(_: Int) {
  }

  // `String` would be a direct result as a struct type
  distributed func simple2(_: Int) -> String {
    return ""
  }

  // `String` is an object that gets exploded into two parameters
  distributed func simple3(_: String) -> Int {
    return 42
  }

  // Enum with a single case are special because they have an empty
  // native schema so they are dropped from parameters/result.
  distributed func single_case_enum(_ e: SimpleE) -> SimpleE {
    return e
  }

  distributed func with_indirect_enums(_: IndirectE, _: Int) -> IndirectE {
    return .empty
  }

  // Combination of multiple arguments, reference type and indirect result
  //
  // Note: Tuple types cannot be used here is either position because they
  // cannot conform to protocols.
  distributed func complex(_: [Int], _: Obj, _: String?, _: LargeStruct) -> LargeStruct {
    fatalError()
  }
}

@available(SwiftStdlib 5.6, *)
public distributed actor MyOtherActor {
  distributed func empty() {
  }
}


/// ---> Let's check that distributed accessors and thunks are emitted as accessible functions

/// -> `MyActor.simple1`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTEHF" = private constant
// CHECK-SAME: @"symbolic Si___________pIetMHygzo_ 27distributed_actor_accessors7MyActorC s5ErrorP"
// CHECK-SAME: (%swift.async_func_pointer* @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTETFTu" to i64)
// CHECK-SAME: , section ".sw5acfn$B", {{.*}}

/// -> `MyActor.simple2`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTEHF" = private constant
// CHECK-SAME: @"symbolic Si_____SS______pIetMHygozo_ 27distributed_actor_accessors7MyActorC s5ErrorP"
// CHECK-SAME: (%swift.async_func_pointer* @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTETFTu" to i64
// CHECK-SAME: , section ".sw5acfn$B", {{.*}}

/// -> `MyActor.simple3`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTEHF" = private constant
// CHECK-SAME: @"symbolic SS_____Si______pIetMHggdzo_ 27distributed_actor_accessors7MyActorC s5ErrorP"
// CHECK-SAME: (%swift.async_func_pointer* @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTETFTu" to i64)
// CHECK-SAME: , section ".sw5acfn$B", {{.*}}

/// -> `MyActor.single_case_enum`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFFTEHF" = private constant
// CHECK-SAME: @"symbolic __________AA______pIetMHygdzo_ 27distributed_actor_accessors7SimpleEO AA7MyActorC s5ErrorP"
// CHECK-SAME: (%swift.async_func_pointer* @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFFTETFTu" to i64)
// CHECK-SAME: , section ".sw5acfn$B", {{.*}}

/// -> `MyActor.with_indirect_enums`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC19with_indirect_enumsyAA9IndirectEOAF_SitFTEHF" = private constant
// CHECK-SAME: @"symbolic _____Si_____AA______pIetMHgygozo_ 27distributed_actor_accessors9IndirectEO AA7MyActorC s5ErrorP"
// CHECK-SAME: (%swift.async_func_pointer* @"$s27distributed_actor_accessors7MyActorC19with_indirect_enumsyAA9IndirectEOAF_SitFTETFTu" to i64
// CHECK-SAME: , section ".sw5acfn$B", {{.*}}

/// -> `MyActor.complex`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtFTEHF" = private constant
// CHECK-SAME: @"symbolic SaySiG_____SSSg__________AD______pIetMHgggngrzo_ 27distributed_actor_accessors3ObjC AA11LargeStructV AA7MyActorC s5ErrorP"
// CHECK-SAME: (%swift.async_func_pointer* @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtFTETFTu" to i64)
// CHECK-SAME: , section ".sw5acfn$B", {{.*}}

/// -> `MyOtherActor.empty`
// CHECK:      @"$s27distributed_actor_accessors12MyOtherActorC5emptyyyFTEHF" = private constant
// CHECK-SAME: @"symbolic ___________pIetMHgzo_ 27distributed_actor_accessors12MyOtherActorC s5ErrorP"
// CHECK-SAME: (%swift.async_func_pointer* @"$s27distributed_actor_accessors12MyOtherActorC5emptyyyFTETFTu" to i64)
// CHECK-SAME: , section ".sw5acfn$B", {{.*}}

// CHECK:      @llvm.used = appending global [{{.*}} x i8*] [
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC19with_indirect_enumsyAA9IndirectEOAF_SitFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors12MyOtherActorC5emptyyyFTEHF"
// CHECK-SAME: ], section "llvm.metadata"
