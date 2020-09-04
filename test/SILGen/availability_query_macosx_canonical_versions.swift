// RUN: %target-swift-emit-sil %s -target %target-cpu-apple-macosx10.50 -verify
// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-macosx10.50 | %FileCheck %s

// REQUIRES: OS=macosx

// CHECK-LABEL: sil{{.+}}@main{{.*}} {

// Test for the runtime non-canonical version hack for canonical macOS versioning.
// This will eventually change to be the correctly canonicalized version.

// CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 16
// CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[FUNC:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(OSX 10.16, iOS 7.1, *) {
}