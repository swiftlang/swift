// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 -enable-experimental-concurrency -enable-actor-data-race-checks | %FileCheck --enable-var-scope %s --check-prefix=CHECK-RAW
// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 -enable-experimental-concurrency -enable-actor-data-race-checks > %t.sil
// RUN: %target-sil-opt -enable-sil-verify-all %t.sil -lower-hop-to-actor -enable-experimental-concurrency | %FileCheck --enable-var-scope %s --check-prefix=CHECK-CANONICAL
// REQUIRES: concurrency

import Swift
import _Concurrency

// CHECK-RAW-LABEL: sil [ossa] @$s4test11onMainActoryyF
// CHECK-RAW: extract_executor [[MAIN_ACTOR:%.*]] : $MainActor

// CHECK-CANONICAL-LABEL: sil [ossa] @$s4test11onMainActoryyF
// CHECK-CANONICAL: function_ref @$ss22_checkExpectedExecutor7Builtin15_filenameLength01_E7IsASCII5_line9_executoryBp_BwBi1_BwBetF
@MainActor public func onMainActor() { }

func takeClosure(_ fn: @escaping () -> Int) { }

func takeUnsafeMainActorClosure(@_unsafeMainActor _ fn: @escaping () -> Int) { }

public actor MyActor {
  var counter = 0

  // CHECK-RAW-LABEL: sil private [ossa] @$s4test7MyActorC10getUpdaterSiycyFSiycfU_
  // CHECK-RAW: extract_executor [[ACTOR:%.*]] : $MyActor

  // CHECK-CANONICAL-LABEL: sil private [ossa] @$s4test7MyActorC10getUpdaterSiycyFSiycfU_
  // CHECK-CANONICAL: function_ref @$ss22_checkExpectedExecutor7Builtin15_filenameLength01_E7IsASCII5_line9_executoryBp_BwBi1_BwBetF
  public func getUpdater() -> (() -> Int) {
    return {
      self.counter = self.counter + 1
      return self.counter
    }
  }

  // CHECK-RAW: sil private [ossa] @$s4test7MyActorCfdSiycfU_
  // CHECK-RAW-NOT: extract_executor
  // CHECK-RAW: return [[VALUE:%.*]] : $Int
  deinit {
    takeClosure { self.counter }
  }

  // CHECK-RAW-LABEL: sil private [ossa] @$s4test7MyActorC0A10UnsafeMainyyFSiycfU_
  // CHECK-RAW-NOT: _checkExpectedExecutor
  // CHECK-RAW: onMainActor
  // CHECK-RAW: return
  public func testUnsafeMain() {
    takeUnsafeMainActorClosure {
      onMainActor()
      return 5
    }
  }
}
