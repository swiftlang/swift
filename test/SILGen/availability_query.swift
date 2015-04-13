// RUN: %target-swift-frontend -emit-silgen -enable-experimental-availability-checking %s | FileCheck %s

// REQUIRES: OS=macosx

// CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 9
// CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 8
// CHECK: [[FUNC:%.*]] = function_ref @_TFSs26_stdlib_isOSVersionAtLeastFTBwBwBw_Bi1_ : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[BOOL_FUNC:%.*]] = function_ref @_TFSs8_getBoolFBi1_Sb : $@convention(thin) (Builtin.Int1) -> Bool
// CHECK: [[BOOL_RESULT:%.*]] = apply [[BOOL_FUNC]]([[QUERY_RESULT]]) : $@convention(thin) (Builtin.Int1) -> Bool
if #available(OSX >= 10.9.8, iOS >= 7.1, *) {
}

// CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 9
// CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[FUNC:%.*]] = function_ref @_TFSs26_stdlib_isOSVersionAtLeastFTBwBwBw_Bi1_ : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[BOOL_FUNC:%.*]] = function_ref @_TFSs8_getBoolFBi1_Sb : $@convention(thin) (Builtin.Int1) -> Bool
// CHECK: [[BOOL_RESULT:%.*]] = apply [[BOOL_FUNC]]([[QUERY_RESULT]]) : $@convention(thin) (Builtin.Int1) -> Bool
// Since we are compiling for an unmentioned platform (OS X), we check against the minimum
// deployment target, which is 10.9
if #available(iOS >= 7.1, *) {
}


// CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[QUERY_FUNC:%.*]] = function_ref @_TFSs26_stdlib_isOSVersionAtLeastFTBwBwBw_Bi1_ : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[BOOL_FUNC:%.*]] = function_ref @_TFSs8_getBoolFBi1_Sb : $@convention(thin) (Builtin.Int1) -> Bool
// CHECK: [[BOOL_RESULT:%.*]] = apply [[BOOL_FUNC]]([[QUERY_RESULT]]) : $@convention(thin) (Builtin.Int1) -> Bool
if #available(OSX >= 10.10, *) {
}

// CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[QUERY_FUNC:%.*]] = function_ref @_TFSs26_stdlib_isOSVersionAtLeastFTBwBwBw_Bi1_ : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[BOOL_FUNC:%.*]] = function_ref @_TFSs8_getBoolFBi1_Sb : $@convention(thin) (Builtin.Int1) -> Bool
// CHECK: [[BOOL_RESULT:%.*]] = apply [[BOOL_FUNC]]([[QUERY_RESULT]]) : $@convention(thin) (Builtin.Int1) -> Bool
if #available(OSX >= 10, *) {
}
