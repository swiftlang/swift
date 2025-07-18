// RUN: %target-swift-emit-silgen -enable-upcoming-feature NonisolatedNonsendingByDefault %s | %FileCheck %s

// REQUIRES: swift_feature_NonisolatedNonsendingByDefault


// CHECK-LABEL: // concurrentTest()
// CHECK: // Isolation: nonisolated
// CHECK: sil hidden [ossa] @$s14attr_execution14concurrentTestyyYaF : $@convention(thin) @async () -> () {
@concurrent
func concurrentTest() async {}

// CHECK-LABEL: // callerTest()
// CHECK: // Isolation: caller_isolation_inheriting
// CHECK: sil hidden [ossa] @$s14attr_execution10callerTestyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
nonisolated(nonsending)
func callerTest() async {}

struct Test {
  // CHECK-LABEL: // closure #1 in variable initialization expression of Test.x
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil private [ossa] @$s14attr_execution4TestV1xyyYaYCcvpfiyyYacfU_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
  var x: () async -> Void = {}

  // CHECK-LABEL: // Test.test()
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil hidden [ossa] @$s14attr_execution4TestV4testyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed Test) -> ()
  // CHECK: bb0([[ISOLATION:%.*]] : @guaranteed $Optional<any Actor>, [[SELF:%.*]] : @guaranteed $Test)
  // CHECK: [[X_REF:%.*]] = struct_extract %1, #Test.x
  // CHECK: [[X_REF_COPY:%.]] = copy_value [[X_REF]]
  // CHECK: [[BORROWED_X:%.*]] = begin_borrow [[X_REF_COPY]]
  // CHECK: apply [[BORROWED_X]]([[ISOLATION]]) : $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
  // CHECK: } // end sil function '$s14attr_execution4TestV4testyyYaF'
  func test() async {
    await x()
  }

  // CHECK-LABEL: // Test.testParam(fn:)
  // CHECK: // Isolation: caller_isolation_inheriting
  // CHECK: sil hidden [ossa] @$s14attr_execution4TestV9testParam2fnyyyYaYCcSg_tYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed Optional<@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()>, @guaranteed Test) -> ()
  // CHECK: bb0([[ISOLATION:%.*]] : @guaranteed $Optional<any Actor>, [[OPT_FN:%.*]] : @guaranteed $Optional<@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()>, [[SELF:%.*]] : @guaranteed $Test)
  // CHECK: bb1([[FN:%.*]] : @owned $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ())
  // CHECK: [[BORROWED_FN:%.*]] = begin_borrow [[FN]]
  // CHECK: apply [[BORROWED_FN]]([[ISOLATION]]) : $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()  
  // CHECK: } // end sil function '$s14attr_execution4TestV9testParam2fnyyyYaYCcSg_tYaF'
  func testParam(fn: (() async -> Void)?) async {
    await fn?()
  }
}

// CHECK-LABEL: // testLocal()
// CHECK: // Isolation: caller_isolation_inheriting
// CHECK: sil hidden [ossa] @$s14attr_execution9testLocalyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// CHECK: bb0([[ISOLATION:%.*]] : @guaranteed $Optional<any Actor>)
// CHECK: bb1([[FN:%.*]] : @owned $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ())
// CHECK: [[BORROWED_FN:%.*]] = begin_borrow [[FN]]
// CHECK: apply [[BORROWED_FN]]([[ISOLATION]]) : $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
// CHECK: } // end sil function '$s14attr_execution9testLocalyyYaF'
func testLocal() async {
  let fn: (() async -> Void)? = nil
  await fn?()
}

// CHECK-LABEL: // takesClosure(fn:)
// CHECK: // Isolation: unspecified
// CHECK: sil hidden [ossa] @$s14attr_execution12takesClosure2fnyyyYaYCXE_tF : $@convention(thin) (@guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> ()
func takesClosure(fn: () async -> Void) {
}

// CHECK-LABEL: sil hidden [ossa] @$s14attr_execution11testClosureyyF : $@convention(thin) () -> ()
// CHECK:  [[CLOSURE:%.*]] = function_ref @$s14attr_execution11testClosureyyFyyYaXEfU_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
// CHECK:  [[THUNKED_CLOSURE:%.*]] = thin_to_thick_function %0 to $@noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
// CHECK:  [[TAKES_CLOSURE:%.*]] = function_ref @$s14attr_execution12takesClosure2fnyyyYaYCXE_tF : $@convention(thin) (@guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> ()
// CHECK:  apply [[TAKES_CLOSURE]]([[THUNKED_CLOSURE]])
// CHECK: } // end sil function '$s14attr_execution11testClosureyyF'

// CHECK-LABEL: // closure #1 in testClosure()
// CHECK: // Isolation: caller_isolation_inheriting
// CHECK: sil private [ossa] @$s14attr_execution11testClosureyyFyyYaXEfU_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
func testClosure() {
  takesClosure {
  }
}

protocol P {
}

func open<T: P>(_: T) async {}

// CHECK-LABEL: sil hidden [ossa] @$s14attr_execution19testOpenExistential11existentialyAA1P_p_tYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed any P) -> ()
// CHECK: bb0([[ISOLATION:%.*]] : @guaranteed $Optional<any Actor>, [[EXISTENTIAL:%.*]] : $*any P):
// CHECK: [[OPEN_REF:%.*]] = function_ref @$s14attr_execution4openyyxYaAA1PRzlF
// CHECK: apply [[OPEN_REF]]<@opened("{{.*}}", any P) Self>([[ISOLATION]], {{.*}})
// CHECK: } // end sil function '$s14attr_execution19testOpenExistential11existentialyAA1P_p_tYaF'
func testOpenExistential(existential: any P) async {
  await _openExistential(existential, do: open)
}

func testWithoutActuallyEscaping(_ f: () async -> ()) async {
  // CHECK-LABEL: // closure #1 in testWithoutActuallyEscaping(_:)
  // CHECK-NEXT: // Isolation: caller_isolation_inheriting
  await withoutActuallyEscaping(f) {
    await $0()
  }

  // CHECK-LABEL: // closure #2 in testWithoutActuallyEscaping(_:)
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  await withoutActuallyEscaping(f) { @MainActor in
    await $0()
  }

  actor Test {
    // CHECK-LABEL: // closure #1 in testActorIsolatedCapture() in Test #1 in testWithoutActuallyEscaping(_:)
    // CHECK-NEXT: // Isolation: actor_instance. name: 'self'
    func testActorIsolatedCapture() async {
      await withoutActuallyEscaping(compute) {
        _ = self
        await $0()
      }
    }

    func compute() async {}
  }
}
