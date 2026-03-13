// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault -verify %s | %FileCheck %s

// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: concurrency
// REQUIRES: objc_interop

import Foundation

class Test: NSObject {

  // CHECK: // Test.testExplicit(_:)
  // CHECK-NEXT: // Isolation: caller_isolation_inheriting
  // CHECK-LABEL: sil hidden [ossa] @$s27nonisolated_nonsending_objc4TestC12testExplicityyySSYbcYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @Sendable @callee_guaranteed (@guaranteed String) -> (), @guaranteed Test) -> ()

  // CHECK: // @objc Test.testExplicit(_:)
  // CHECK-NEXT: // Isolation: caller_isolation_inheriting
  // CHECK-LABEL: sil private [thunk] [ossa] @$s27nonisolated_nonsending_objc4TestC12testExplicityyySSYbcYaFTo : $@convention(objc_method) (@convention(block) @Sendable (NSString) -> (), @convention(block) () -> (), Test) -> ()

  // @objc closure #1 in Test.testExplicit(_:)
  // CHECK-LABEL: sil shared [thunk] [ossa] @$s27nonisolated_nonsending_objc4TestC12testExplicityyySSYbcYaFyyYacfU_To : $@convention(thin) @Sendable @async (@convention(block) @Sendable (NSString) -> (), @convention(block) () -> (), Test) -> () {
  // CHECK: [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
  // CHECK: [[BUILTIN_ACTOR:%.*]] = unchecked_value_cast [[ACTOR]] to $Builtin.ImplicitActor
  // CHECK: [[TEST_EXPLICIT_REF:%.*]] = function_ref @$s27nonisolated_nonsending_objc4TestC12testExplicityyySSYbcYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @Sendable @callee_guaranteed (@guaranteed String) -> (), @guaranteed Test) -> ()
  // CHECK-NEXT: apply [[TEST_EXPLICIT_REF]]([[BUILTIN_ACTOR]], {{.*}}) : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @Sendable @callee_guaranteed (@guaranteed String) -> (), @guaranteed Test) -> ()
  // CHECK: } // end sil function '$s27nonisolated_nonsending_objc4TestC12testExplicityyySSYbcYaFyyYacfU_To'
  @objc nonisolated(nonsending) func testExplicit(_: @Sendable @escaping (String) -> Void) async {
  }

  // CHECK: // Test.testImplicit(_:)
  // CHECK-NEXT: // Isolation: caller_isolation_inheriting
  // CHECK-LABEL: sil hidden [ossa] @$s27nonisolated_nonsending_objc4TestC12testImplicityyySSYbcYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @Sendable @callee_guaranteed (@guaranteed String) -> (), @guaranteed Test) -> ()

  // CHECK: // @objc Test.testImplicit(_:)
  // CHECK-NEXT: // Isolation: caller_isolation_inheriting
  // CHECK-LABEL: sil private [thunk] [ossa] @$s27nonisolated_nonsending_objc4TestC12testImplicityyySSYbcYaFTo : $@convention(objc_method) (@convention(block) @Sendable (NSString) -> (), @convention(block) () -> (), Test) -> ()

  // @objc closure #1 in Test.testImplicit(_:)
  // CHECK-LABEL: sil shared [thunk] [ossa] @$s27nonisolated_nonsending_objc4TestC12testImplicityyySSYbcYaFyyYacfU_To : $@convention(thin) @Sendable @async (@convention(block) @Sendable (NSString) -> (), @convention(block) () -> (), Test) -> () {
  // CHECK: [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
  // CHECK: [[BUILTIN_ACTOR:%.*]] = unchecked_value_cast [[ACTOR]] to $Builtin.ImplicitActor
  // CHECK: [[TEST_IMPLICIT_REF:%.*]] = function_ref @$s27nonisolated_nonsending_objc4TestC12testImplicityyySSYbcYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @Sendable @callee_guaranteed (@guaranteed String) -> (), @guaranteed Test) -> ()
  // CHECK-NEXT: apply [[TEST_IMPLICIT_REF]]([[BUILTIN_ACTOR]], {{.*}}) : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @Sendable @callee_guaranteed (@guaranteed String) -> (), @guaranteed Test) -> ()
  // CHECK: } // end sil function '$s27nonisolated_nonsending_objc4TestC12testImplicityyySSYbcYaFyyYacfU_To'
  @objc func testImplicit(_: @Sendable @escaping (String) -> Void) async {
  }
}
