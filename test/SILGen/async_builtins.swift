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

  // CHECK-LABEL: sil hidden [ossa] @$s4test1XV12launchFutureyyxlF : $@convention(method) <T> (@in_guaranteed T, X) -> ()
  func launchFuture<T>(_ value: T) {
    // CHECK: builtin "createAsyncTaskFuture"<T>([[ZERO:%.*]] : $Int, [[NIL:%.*]] : $Optional<Builtin.NativeObject>, [[FN:%.*]] : $@async @callee_guaranteed @substituted <τ_0_0> () -> (@out τ_0_0, @error Error) for <T>) : $(Builtin.NativeObject, Builtin.RawPointer)
    let task = Builtin.createAsyncTaskFuture(0, nil) { () async throws -> T in
      return value
    }
  }

  func launchFuture<T>(_ value: T) {
    // CHECK: builtin "createAsyncTaskGroup"<T>([[ZERO:%.*]] : $Int, [[NIL:%.*]] : $Optional<Builtin.NativeObject>) : $(Builtin.NativeObject, Builtin.RawPointer)
    let task = Builtin.createAsyncTaskGroup(0, nil)
  }

  public func launchRocker<T>(closure: @escaping () async throws -> T) {
    _ = Builtin.createAsyncTaskFuture(0, nil, closure)
  }
}
