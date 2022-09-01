// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift

// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_dist -disable-availability-checking -I %t %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir -disable-availability-checking -I %t %s

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor MyDistActor {
  init(system: FakeActorSystem) {
    self.actorSystem = system
  }

  // CHECK-LABEL: sil hidden @$s13coverage_dist11MyDistActorCfd : $@convention(method) (@guaranteed MyDistActor) -> @owned Builtin.NativeObject
  // CHECK:       cond_br {{%[0-9]+}}, {{bb[0-9]+}}, [[DEINITBODYBB:bb[0-9]+]]

  // CHECK:       [[DEINITBODYBB]]:
  // CHECK-NEXT:  string_literal utf8
  // CHECK-NEXT:  integer_literal $Builtin.Int64, 0
  // CHECK-NEXT:  integer_literal $Builtin.Int32, 2
  // CHECK-NEXT:  integer_literal $Builtin.Int32, 0
  // CHECK-NEXT:  int_instrprof_increment
  // CHECK:       function_ref @$sSb6randomSbyFZ
  // CHECK:       cond_br {{%[0-9]+}}, [[TRUEBB:bb[0-9]+]], {{bb[0-9]+}}

  // CHECK:       [[TRUEBB]]:
  // CHECK-NEXT:  string_literal utf8
  // CHECK-NEXT:  integer_literal $Builtin.Int64, 0
  // CHECK-NEXT:  integer_literal $Builtin.Int32, 2
  // CHECK-NEXT:  integer_literal $Builtin.Int32, 1
  // CHECK-NEXT:  int_instrprof_increment

  // CHECK-LABEL: sil_coverage_map {{.*}} "$s13coverage_dist11MyDistActorCfd" {{.*}} // coverage_dist.MyDistActor.deinit
  // CHECK-NEXT:  [[@LINE+3]]:10 -> [[@LINE+5]]:4 : 0
  // CHECK-NEXT:  [[@LINE+3]]:25 -> [[@LINE+3]]:26 : 1
  // CHECK-NEXT:  [[@LINE+2]]:29 -> [[@LINE+2]]:30 : (0 - 1)
  deinit {
    let _ = .random() ? 0 : 1
  }
}
