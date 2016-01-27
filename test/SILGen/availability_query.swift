// RUN: %target-swift-frontend -emit-silgen %s -target x86_64-apple-macosx10.50 | FileCheck %s

// REQUIRES: OS=macosx

// CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 53
// CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 8
// CHECK: [[FUNC:%.*]] = function_ref @_TFs26_stdlib_isOSVersionAtLeastFTBwBwBw_Bi1_ : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(OSX 10.53.8, iOS 7.1, *) {
}

// CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 50
// CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[FUNC:%.*]] = function_ref @_TFs26_stdlib_isOSVersionAtLeastFTBwBwBw_Bi1_ : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// Since we are compiling for an unmentioned platform (OS X), we check against the minimum
// deployment target, which is 10.50
if #available(iOS 7.1, *) {
}


// CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 52
// CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[QUERY_FUNC:%.*]] = function_ref @_TFs26_stdlib_isOSVersionAtLeastFTBwBwBw_Bi1_ : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(OSX 10.52, *) {
}

// CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[QUERY_FUNC:%.*]] = function_ref @_TFs26_stdlib_isOSVersionAtLeastFTBwBwBw_Bi1_ : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(OSX 10, *) {
}
