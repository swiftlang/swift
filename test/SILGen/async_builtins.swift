// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 -enable-experimental-concurrency -parse-stdlib -sil-verify-all | %FileCheck %s
// REQUIRES: concurrency

struct X {
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
}
