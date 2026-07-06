// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name test -swift-version 5  -target %target-swift-5.1-abi-triple -enable-actor-data-race-checks | %FileCheck --enable-var-scope %s --check-prefix=CHECK-RAW
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name test -swift-version 5  -target %target-swift-5.1-abi-triple -enable-actor-data-race-checks > %t.sil
// RUN: %target-sil-opt -sil-print-types -enable-sil-verify-all %t.sil -lower-hop-to-actor  | %FileCheck --enable-var-scope %s --check-prefix=CHECK-CANONICAL
// REQUIRES: concurrency

import Swift
import _Concurrency

// CHECK-RAW-LABEL: sil [ossa] @$s4test11onMainActoryyF
// CHECK-RAW: extract_executor [[MAIN_ACTOR:%.*]] : $MainActor

// CHECK-CANONICAL-LABEL: sil [ossa] @$s4test11onMainActoryyF
// CHECK-CANONICAL: function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
@MainActor public func onMainActor() { }

// CHECK-CANONICAL-LABEL: sil [ossa] @$s4test17onMainActorUnsafeyyF
// CHECK-CANONICAL: function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
@preconcurrency @MainActor public func onMainActorUnsafe() { }

func takeClosure(_ fn: @escaping () -> Int) { }

@preconcurrency func takeUnsafeMainActorClosure(_ fn: @MainActor @escaping () -> Int) { }

public actor MyActor {
  var counter = 0

  // CHECK-RAW-LABEL: sil private [ossa] @$s4test7MyActorC10getUpdaterSiycyFSiycfU_
  // CHECK-RAW: extract_executor [[ACTOR:%.*]] : $MyActor

  // CHECK-CANONICAL-LABEL: sil private [ossa] @$s4test7MyActorC10getUpdaterSiycyFSiycfU_
  // CHECK-CANONICAL: function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
  public func getUpdater() -> (() -> Int) {
    return {
      self.counter = self.counter + 1
      return self.counter
    }
  }

  // CHECK-RAW-LABEL: sil private [ossa] @$s4test7MyActorC0A10UnsafeMainyyFSiyScMYccfU_
  // CHECK-RAW: _checkExpectedExecutor
  // CHECK-RAW: onMainActor
  // CHECK-RAW: return
  public func testUnsafeMain() {
    takeUnsafeMainActorClosure {
      onMainActor()
      return 5
    }
  }

  // CHECK-CANONICAL-LABEL: sil private [ossa] @$s4test7MyActorC0A13LocalFunctionyyF5localL_SiyF : $@convention(thin) (@sil_isolated @guaranteed MyActor) -> Int
  // CHECK-CANONICAL: [[CAPTURE:%.*]] = copy_value %0 : $MyActor
  // CHECK-CANONICAL-NEXT: [[BORROWED_CAPTURE:%.*]] = begin_borrow [[CAPTURE]] : $MyActor
  // CHECK-CANONICAL-NEXT: [[EXECUTOR:%.*]] = builtin "buildDefaultActorExecutorRef"<MyActor>([[BORROWED_CAPTURE]] : $MyActor) : $Builtin.Executor
  // CHECK-CANONICAL-NEXT: [[EXECUTOR_DEP:%.*]] = mark_dependence [[EXECUTOR]] : $Builtin.Executor on [[BORROWED_CAPTURE]] : $MyActor
  // CHECK-CANONICAL: [[CHECK_FN:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
  // CHECK-CANONICAL-NEXT: apply [[CHECK_FN]]({{.*}}, [[EXECUTOR_DEP]])
  public func testLocalFunction() {
    func local() -> Int {
      return counter
    }

    print(local())
  }
}
