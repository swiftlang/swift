// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 -enable-experimental-concurrency -parse-stdlib -sil-verify-all | %FileCheck %s
// REQUIRES: concurrency

import Swift

public struct X {
  // CHECK-LABEL: sil hidden [ossa] @$s4test1XV14getCurrentTaskBoyYF
  func getCurrentTask() async -> Builtin.NativeObject {
    // CHECK: builtin "getCurrentAsyncTask"() : $Builtin.NativeObject
    return Builtin.getCurrentAsyncTask()
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test1XV8doCancel4taskyBo_tF : $@convention(method) (@guaranteed Builtin.NativeObject, X) -> ()
  func doCancel(task: Builtin.NativeObject) {
    // CHECK: builtin "cancelAsyncTask"(%0 : $Builtin.NativeObject) : $()
    Builtin.cancelAsyncTask(task)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test1XV10launchTaskyyYF : $@convention(method) @async (X) -> ()
  func launchTask() async {
    // CHECK: builtin "createAsyncTask"([[FLAGS:%.*]] : $Int, [[PARENT:%.*]] : $Optional<Builtin.NativeObject>, [[FN:%.*]] : $@async @callee_guaranteed () -> @error Error) : $(Builtin.NativeObject, Builtin.RawPointer)
    let task = Builtin.getCurrentAsyncTask()
    let childTask = Builtin.createAsyncTask(0, task) {
      await launchTask()
      print("child is done")
    }
  }
}
