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

  public func launchRocker<T>(closure: @escaping () async throws -> T) {
    _ = Builtin.createAsyncTaskFuture(0, nil, closure)
  }
}

// CHECK-LABEL: sil [ossa] @$s4test26usesWithUnsafeContinuationyyYF : $@convention(thin) @async () -> () {
public func usesWithUnsafeContinuation() async {
  // trivial resume type
  let _: Int = await Builtin.withUnsafeContinuation { c in }

  // CHECK: [[FN:%.*]] = function_ref @$s4test26usesWithUnsafeContinuationyyYFyBcXEfU_ : $@convention(thin) (Builtin.RawUnsafeContinuation) -> ()
  // CHECK: [[TMP:%.*]] = convert_function [[FN]] : $@convention(thin) (Builtin.RawUnsafeContinuation) -> () to $@convention(thin) @noescape (Builtin.RawUnsafeContinuation) -> ()
  // CHECK: [[CLOSURE:%.*]] = thin_to_thick_function [[TMP]]
  // CHECK: [[BOX:%.*]] = alloc_stack $Int
  // CHECK: [[CC:%.*]] = get_async_continuation_addr Int, [[BOX]] : $*Int
  // CHECK: apply [[CLOSURE]]([[CC]]) : $@noescape @callee_guaranteed (Builtin.RawUnsafeContinuation) -> ()
  // CHECK: await_async_continuation [[CC]] : $Builtin.RawUnsafeContinuation, resume bb1

  // CHECK: bb1:
  // CHECK: [[RESULT:%.*]] = load [trivial] [[BOX]] : $*Int
  // CHECK: dealloc_stack [[BOX]]

  // loadable resume type
  let _: String = await Builtin.withUnsafeContinuation { c in }

  // CHECK: [[FN:%.*]] = function_ref @$s4test26usesWithUnsafeContinuationyyYFyBcXEfU0_ : $@convention(thin) (Builtin.RawUnsafeContinuation) -> ()
  // CHECK: [[TMP:%.*]] = convert_function [[FN]] : $@convention(thin) (Builtin.RawUnsafeContinuation) -> () to $@convention(thin) @noescape (Builtin.RawUnsafeContinuation) -> ()
  // CHECK: [[CLOSURE:%.*]] = thin_to_thick_function [[TMP]]
  // CHECK: [[BOX:%.*]] = alloc_stack $String
  // CHECK: [[CC:%.*]] = get_async_continuation_addr String, [[BOX]] : $*String
  // CHECK: apply [[CLOSURE]]([[CC]]) : $@noescape @callee_guaranteed (Builtin.RawUnsafeContinuation) -> ()
  // CHECK: await_async_continuation [[CC]] : $Builtin.RawUnsafeContinuation, resume bb2

  // CHECK: bb2:
  // CHECK: [[RESULT:%.*]] = load [take] [[BOX]] : $*String
  // CHECK: destroy_value [[RESULT]]
  // CHECK: dealloc_stack [[BOX]]

  // address-only resume type
  let _: Any = await Builtin.withUnsafeContinuation { c in }

  // CHECK: [[FN:%.*]] = function_ref @$s4test26usesWithUnsafeContinuationyyYFyBcXEfU1_ : $@convention(thin) (Builtin.RawUnsafeContinuation) -> ()
  // CHECK: [[TMP:%.*]] = convert_function [[FN]] : $@convention(thin) (Builtin.RawUnsafeContinuation) -> () to $@convention(thin) @noescape (Builtin.RawUnsafeContinuation) -> ()
  // CHECK: [[CLOSURE:%.*]] = thin_to_thick_function [[TMP]]
  // CHECK: [[BOX:%.*]] = alloc_stack $Any
  // CHECK: [[CC:%.*]] = get_async_continuation_addr Any, [[BOX]] : $*Any
  // CHECK: apply [[CLOSURE]]([[CC]]) : $@noescape @callee_guaranteed (Builtin.RawUnsafeContinuation) -> ()
  // CHECK: await_async_continuation [[CC]] : $Builtin.RawUnsafeContinuation, resume bb3

  // CHECK: bb3:
  // CHECK: [[COPY:%.*]] = alloc_stack $Any
  // CHECK: copy_addr [take] [[BOX]] to [initialization] [[COPY]]
  // CHECK: destroy_addr [[COPY]]
  // CHECK: dealloc_stack [[COPY]]
  // CHECK: dealloc_stack [[BOX]]
}

// CHECK-LABEL: sil [ossa] @$s4test34usesWithUnsafeThrowingContinuationyyYKF : $@convention(thin) @async () -> @error Error {
public func usesWithUnsafeThrowingContinuation() async throws {
  let _: Int = await try Builtin.withUnsafeThrowingContinuation { c in }

  // CHECK: [[FN:%.*]] = function_ref @$s4test34usesWithUnsafeThrowingContinuationyyYKFyBcXEfU_ : $@convention(thin) (Builtin.RawUnsafeContinuation) -> ()
  // CHECK: [[TMP:%.*]] = convert_function [[FN]] : $@convention(thin) (Builtin.RawUnsafeContinuation) -> () to $@convention(thin) @noescape (Builtin.RawUnsafeContinuation) -> ()
  // CHECK: [[CLOSURE:%.*]] = thin_to_thick_function [[TMP]]
  // CHECK: [[BOX:%.*]] = alloc_stack $Int
  // CHECK: [[CC:%.*]] = get_async_continuation_addr [throws] Int, [[BOX]] : $*Int
  // CHECK: apply [[CLOSURE]]([[CC]]) : $@noescape @callee_guaranteed (Builtin.RawUnsafeContinuation) -> ()
  // CHECK: await_async_continuation [[CC]] : $Builtin.RawUnsafeContinuation, resume bb1, error bb2

  // CHECK: bb1:
  // CHECK: [[RESULT:%.*]] = load [trivial] [[BOX]] : $*Int
  // CHECK: dealloc_stack [[BOX]]

  // CHECK: bb2([[ERROR:%.*]] : @owned $Error):
  // CHECK: builtin "willThrow"([[ERROR]] : $Error) : $()
  // CHECK: dealloc_stack [[BOX]]
  // CHECK: throw [[ERROR]]
}

// Make sure we do the right thing when the closure value is non-trivial,
// because it has captures and was formed by a partial_apply.
public func usesWithUnsafeContinuationCaptures(fn: (Builtin.RawUnsafeContinuation) -> ()) async throws {
  let _: Int = await Builtin.withUnsafeContinuation { c in fn(c) }
}
