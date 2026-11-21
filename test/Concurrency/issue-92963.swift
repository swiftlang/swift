// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-silgen -parse-as-library -swift-version 6 -module-name test %t/main.swift | %FileCheck %s
// RUN: %target-swift-frontend -emit-module -parse-as-library -swift-version 6 -module-name Base -o %t %t/Base.swift
// RUN: %target-swift-frontend -emit-silgen -parse-as-library -swift-version 6 -module-name client -I %t %t/client.swift | %FileCheck %s --check-prefix=CLIENT

// REQUIRES: concurrency

// https://github.com/swiftlang/swift/issues/92963
//
// An inherited initializer used to lose a '#isolation' default argument, and
// calling it crashed with "default value not provided yet".

//--- main.swift

class A {
  init(isolation: isolated (any Actor)? = #isolation) {}
}

class B: A {}

// CHECK-LABEL: sil hidden [ossa] @$s4test17nonisolatedCalleryyF :
// CHECK:         [[ISO:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK:         [[INIT:%.*]] = function_ref @$s4test1BC9isolationACScA_pSgYi_tcfC
// CHECK:         apply [[INIT]]([[ISO]], {{%.*}})
// CHECK:       } // end sil function '$s4test17nonisolatedCalleryyF'
func nonisolatedCaller() {
  _ = B()
}

actor MyActor {
  // CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC11actorCalleryyF :
  // CHECK:       bb0([[SELF:%.*]] : @guaranteed $MyActor):
  // CHECK:         [[COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[EXIST:%.*]] = init_existential_ref [[COPY]]
  // CHECK:         [[ISO:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[EXIST]]
  // CHECK:         [[INIT:%.*]] = function_ref @$s4test1BC9isolationACScA_pSgYi_tcfC
  // CHECK:         apply [[INIT]]([[ISO]], {{%.*}})
  // CHECK:       } // end sil function '$s4test7MyActorC11actorCalleryyF'
  func actorCaller() {
    _ = B()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test15mainActorCalleryyF :
// CHECK:         [[SHARED:%.*]] = function_ref @$sScM6sharedScMvgZ
// CHECK:         [[MAIN:%.*]] = apply [[SHARED]]
// CHECK:         [[EXIST:%.*]] = init_existential_ref [[MAIN]]
// CHECK:         [[ISO:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[EXIST]]
// CHECK:         [[INIT:%.*]] = function_ref @$s4test1BC9isolationACScA_pSgYi_tcfC
// CHECK:         apply [[INIT]]([[ISO]], {{%.*}})
// CHECK:       } // end sil function '$s4test15mainActorCalleryyF'
@MainActor func mainActorCaller() {
  _ = B()
}

//--- Base.swift

open class Base {
  public init(isolation: isolated (any Actor)? = #isolation) {}
}

//--- client.swift

import Base

class Sub: Base {}

// CLIENT-LABEL: sil hidden [ossa] @$s6client15mainActorCalleryyF :
// CLIENT:         [[SHARED:%.*]] = function_ref @$sScM6sharedScMvgZ
// CLIENT:         [[MAIN:%.*]] = apply [[SHARED]]
// CLIENT:         [[EXIST:%.*]] = init_existential_ref [[MAIN]]
// CLIENT:         [[ISO:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[EXIST]]
// CLIENT:         apply {{%.*}}([[ISO]], {{%.*}})
// CLIENT:       } // end sil function '$s6client15mainActorCalleryyF'
@MainActor func mainActorCaller() {
  _ = Sub()
}
