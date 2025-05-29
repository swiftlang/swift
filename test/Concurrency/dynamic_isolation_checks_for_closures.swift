// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library API
// RUN: %target-swift-frontend -emit-module %t/src/API.swift \
// RUN:   -module-name API -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/API.swiftmodule \
// RUN:   -emit-module-interface-path %t/API.swiftinterface

// Build client with module
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types \
// RUN:   -I %t \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -module-name Client \
// RUN:   -swift-version 6 \
// RUN:    %t/src/Client.swift -verify | %FileCheck %s

//--- API.swift
public func compute(_: (() -> Void)?) {}

public func computeSendable(_: @escaping @Sendable () -> Void) {}

public func computeSending(_: sending @escaping () -> Void) {}

//--- Client.swift
import API

func localCompute(_: (() -> Void)?) {}

func forceIsolation(isolation: isolated (any Actor)?) {}

// CHECK-LABEL: sil private [ossa] @$s6Client17test_global_actoryyFyycfU_ : $@convention(thin) () -> ()
// CHECK: [[EXPECTED_EXECUTOR:%.*]] = extract_executor {{.*}} : $MainActor
// CHECK: [[CHECK_EXECUTOR_FN:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK: %11 = apply [[CHECK_EXECUTOR_FN]]({{.*}}, [[EXPECTED_EXECUTOR]]) : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK: } // end sil function '$s6Client17test_global_actoryyFyycfU_'
@MainActor
func test_global_actor() {
  compute {
  }
}

// CHECK-LABEL: sil private [ossa] @$s6Client13test_isolated9isolationyScMYi_tFyycfU_ : $@convention(thin) (@sil_isolated @guaranteed MainActor) -> ()
// CHECK: [[EXPECTED_EXECUTOR:%.*]] = extract_executor {{.*}} : $MainActor
// CHECK: [[CHECK_EXECUTOR_FN:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK: %11 = apply [[CHECK_EXECUTOR_FN]]({{.*}}, [[EXPECTED_EXECUTOR]]) : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK: } // end sil function '$s6Client13test_isolated9isolationyScMYi_tFyycfU_'
func test_isolated(isolation: isolated MainActor) {
  compute {
    forceIsolation(isolation: isolation)
  }
}

// CHECK-LABEL: sil private [ossa] @$s6Client55test_concurrency_checked_global_isolation_has_no_checksyyFyycfU_ : $@convention(thin) () -> ()
// CHECK-NOT: function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
// CHECK: } // end sil function '$s6Client55test_concurrency_checked_global_isolation_has_no_checksyyFyycfU_'
@MainActor
func test_concurrency_checked_global_isolation_has_no_checks() {
  localCompute {
  }
}

// CHECK-LABEL: sil private [ossa] @$s6Client47test_concurrency_checked_isolated_has_no_checks9isolationyScMYi_tFyycfU_ : $@convention(thin) (@sil_isolated @guaranteed MainActor) -> ()
// CHECK-NOT: function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
// CHECK: } // end sil function '$s6Client47test_concurrency_checked_isolated_has_no_checks9isolationyScMYi_tFyycfU_'
func test_concurrency_checked_isolated_has_no_checks(isolation: isolated MainActor) {
  localCompute {
    forceIsolation(isolation: isolation)
  }
}

// CHECK-LABEL: sil private [ossa] @$s6Client24test_direct_closure_callyyFyyXEfU_ : $@convention(thin) () -> ()
// CHECK-NOT: function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
// CHECK: [[INNER_CLOSURE_REF:%.*]] = function_ref @$s6Client24test_direct_closure_callyyFyyXEfU_yycfU_ : $@convention(thin) () -> ()
// CHECK-NEXT: [[INNER_CLOSURE:%.*]] = thin_to_thick_function [[INNER_CLOSURE_REF]] : $@convention(thin) () -> () to $@callee_guaranteed () -> ()
// CHECK-NEXT: [[OPT_INNER_CLOSURE:%.*]] = enum $Optional<@callee_guaranteed () -> ()>, #Optional.some!enumelt, [[INNER_CLOSURE]] : $@callee_guaranteed () -> ()
// CHECK: [[COMPUTE_FN:%.*]] = function_ref @$s3API7computeyyyycSgF : $@convention(thin) (@guaranteed Optional<@callee_guaranteed () -> ()>) -> ()
// CHECK-NEXT: apply [[COMPUTE_FN]]([[OPT_INNER_CLOSURE]]) : $@convention(thin) (@guaranteed Optional<@callee_guaranteed () -> ()>) -> ()
// CHECK: } // end sil function '$s6Client24test_direct_closure_callyyFyyXEfU_'

// CHECK-LABEL: sil private [ossa] @$s6Client24test_direct_closure_callyyFyyXEfU_yycfU_ : $@convention(thin) () -> ()
// CHECK: [[EXPECTED_EXECUTOR:%.*]] = extract_executor {{.*}} : $MainActor
// CHECK: [[CHECK_EXECUTOR_FN:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK: %11 = apply [[CHECK_EXECUTOR_FN]]({{.*}}, [[EXPECTED_EXECUTOR]]) : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK: } // end sil function '$s6Client24test_direct_closure_callyyFyyXEfU_yycfU_'
@MainActor
func test_direct_closure_call() {
  {
    compute {
    }
  }()
}

@MainActor
func test_global_actor_sendable_and_sending_closures() {
  // CHECK-LABEL: sil private [ossa] @$s6Client47test_global_actor_sendable_and_sending_closuresyyFyyYbcfU_ : $@convention(thin) @Sendable () -> ()
  // CHECK-NOT: function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
  // CHECK: } // end sil function '$s6Client47test_global_actor_sendable_and_sending_closuresyyFyyYbcfU_'
  computeSendable {   
  }

  // CHECK-LABEL: sil private [ossa] @$s6Client47test_global_actor_sendable_and_sending_closuresyyFyycfU0_ : $@convention(thin) () -> ()
  // CHECK-NOT: function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
  // CHECK: } // end sil function '$s6Client47test_global_actor_sendable_and_sending_closuresyyFyycfU0_'
  computeSending {
    forceIsolation(isolation: #isolation)
  }
}
