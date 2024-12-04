// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -emit-irgen -module-name distributed_actor_accessors -target %target-swift-5.7-abi-triple -I %t 2>&1 %s | %IRGenFileCheck %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

// REQUIRES: VENDOR=apple

import Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.7, *)
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

@available(SwiftStdlib 5.7, *)
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

  // Make sure that Sendable doesn't show up in the mangled name
  distributed func generic<T: Codable & Sendable>(_: T) {
  }
}

@available(SwiftStdlib 5.7, *)
public distributed actor MyOtherActor {
  distributed func empty() {
  }
}


/// ---> Let's check that distributed accessors and thunks are emitted as accessible functions

/// -> `MyActor.simple1`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7simple1yySiYaKFTEHF" = private constant
// CHECK-SAME: @"symbolic Si___________pIetMHyTgzo_ 27distributed_actor_accessors7MyActorC s5ErrorP"
// CHECK-SAME: (ptr @"$s27distributed_actor_accessors7MyActorC7simple1yySiYaKFTETFTu" to i{{32|64}})
// CHECK-SAME: , section {{"swift5_accessible_functions"|".sw5acfn$B"|"__TEXT, __swift5_acfuncs, regular"}}

/// -> `MyActor.simple2`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiYaKFTEHF" = private constant
// CHECK-SAME: @"symbolic Si_____SS______pIetMHyTgozo_ 27distributed_actor_accessors7MyActorC s5ErrorP"
// CHECK-SAME: (ptr @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiYaKFTETFTu" to i{{32|64}})
// CHECK-SAME: , section {{"swift5_accessible_functions"|".sw5acfn$B"|"__TEXT, __swift5_acfuncs, regular"}}

/// -> `MyActor.simple3`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSYaKFTEHF" = private constant
// CHECK-SAME: @"symbolic SS_____Si______pIetMHgTgdzo_ 27distributed_actor_accessors7MyActorC s5ErrorP"
// CHECK-SAME: (ptr @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSYaKFTETFTu" to i{{32|64}})
// CHECK-SAME: , section {{"swift5_accessible_functions"|".sw5acfn$B"|"__TEXT, __swift5_acfuncs, regular"}}

/// -> `MyActor.single_case_enum`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFYaKFTEHF" = private constant
// CHECK-SAME: @"symbolic __________AA______pIetMHyTgdzo_ 27distributed_actor_accessors7SimpleEO AA7MyActorC s5ErrorP"
// CHECK-SAME: (ptr @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFYaKFTETFTu" to i{{32|64}})
// CHECK-SAME: , section {{"swift5_accessible_functions"|".sw5acfn$B"|"__TEXT, __swift5_acfuncs, regular"}}

/// -> `MyActor.with_indirect_enums`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC19with_indirect_enumsyAA9IndirectEOAF_SitYaKFTEHF" = private constant
// CHECK-SAME: @"symbolic _____Si_____AA______pIetMHgTyTgozo_ 27distributed_actor_accessors9IndirectEO AA7MyActorC s5ErrorP"
// CHECK-SAME: (ptr @"$s27distributed_actor_accessors7MyActorC19with_indirect_enumsyAA9IndirectEOAF_SitYaKFTETFTu" to i{{32|64}})
// CHECK-SAME: , section {{"swift5_accessible_functions"|".sw5acfn$B"|"__TEXT, __swift5_acfuncs, regular"}}

/// -> `MyActor.complex`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtYaKFTEHF" = private constant
// CHECK-SAME: @"symbolic SaySiG_____SSSg__________AD______pIetMHgTgTgTnTgrzo_ 27distributed_actor_accessors3ObjC AA11LargeStructV AA7MyActorC s5ErrorP"
// CHECK-SAME: (ptr @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtYaKFTETFTu" to i{{32|64}})
// CHECK-SAME: , section {{"swift5_accessible_functions"|".sw5acfn$B"|"__TEXT, __swift5_acfuncs, regular"}}

/// -> `MyActor.generic`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7genericyyxYaKSeRzSERzlFTEHF" = private constant
// CHECK-SAME: @"symbolic x___________pSeRzSERzlIetMHnTgzo_ 27distributed_actor_accessors7MyActorC s5ErrorP"
// CHECK-SAME: (ptr @"$s27distributed_actor_accessors7MyActorC7genericyyxYaKSeRzSERzlFTETFTu" to i{{32|64}})
// CHECK-SAME: , section {{"swift5_accessible_functions"|".sw5acfn$B"|"__TEXT, __swift5_acfuncs, regular"}}

/// -> `MyOtherActor.empty`
// CHECK:      @"$s27distributed_actor_accessors12MyOtherActorC5emptyyyYaKFTEHF" = private constant
// CHECK-SAME: @"symbolic ___________pIetMHgzo_ 27distributed_actor_accessors12MyOtherActorC s5ErrorP"
// CHECK-SAME: (ptr @"$s27distributed_actor_accessors12MyOtherActorC5emptyyyYaKFTETFTu" to i{{32|64}})
// CHECK-SAME: , section {{"swift5_accessible_functions"|".sw5acfn$B"|"__TEXT, __swift5_acfuncs, regular"}}

// CHECK:      @llvm.used = appending global [{{.*}} x ptr] [
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7simple1yySiYaKFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiYaKFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSYaKFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFYaKFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC19with_indirect_enumsyAA9IndirectEOAF_SitYaKFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtYaKFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors12MyOtherActorC5emptyyyYaKFTEHF"
// CHECK-SAME: ], section "llvm.metadata"
