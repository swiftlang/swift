// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature OnceFunctions -swift-version 6 %s | %FileCheck %s
// RUN: %target-swift-frontend -dump-ast -enable-experimental-feature OnceFunctions -swift-version 6 %s | %FileCheck %s --check-prefix=AST
// RUN: %target-swift-frontend -emit-sil -verify -sil-verify-all -enable-experimental-feature OnceFunctions -swift-version 6 %s -o /dev/null

// REQUIRES: swift_feature_OnceFunctions

// withoutActuallyDiscarding(_:do:) calls the body with the argument converted
// by a marked convert_function.

// AST-LABEL: func_decl{{.*}}"discard(_:)"
// AST: function_conversion_expr{{.*}}without_actually_discarding

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}7discard{{.*}}F : $@convention(thin) (@owned @noescape @once @callee_owned () -> ()) -> () {
// CHECK: bb0([[F:%[0-9]+]] : @owned $@noescape @once @callee_owned () -> ()):
// CHECK:   [[CONVERTED:%[0-9]+]] = convert_function {{%[0-9]+}} : $@noescape @once @callee_owned () -> () to [without_actually_discarding] $@noescape @callable(once) @callee_owned () -> ()
// CHECK:   apply {{%[0-9]+}}([[CONVERTED]])
func discard(_ f: consuming @once () -> Void) {
  withoutActuallyDiscarding(f) { g in g() }
}

// The body parameter is escaping if the argument is.
// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}15discardEscaping{{.*}}F : $@convention(thin) (@owned @once @callee_owned () -> ()) -> () {
// CHECK: convert_function {{%[0-9]+}} : $@once @callee_owned () -> () to [without_actually_discarding] $@callable(once) @callee_owned () -> ()
func discardEscaping(_ f: consuming @escaping @once () -> Void) {
  withoutActuallyDiscarding(f) { g in g() }
}
