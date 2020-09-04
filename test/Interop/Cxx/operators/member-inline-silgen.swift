// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import MemberInline

public func add(_ lhs: inout IntBox, _ rhs: IntBox) -> IntBox { lhs + rhs }

// CHECK: bb0([[SELF:%.*]] : $*IntBox, [[RHS:%.*]] : $IntBox):

// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*IntBox
// CHECK: [[OP:%.*]] = function_ref [[NAME:@(_ZN6IntBoxplES_|\?\?HIntBox@@QEAA\?AU0@U0@@Z)]] : $@convention(c) (@inout IntBox, IntBox) -> IntBox
// CHECK: apply [[OP]]([[SELFACCESS]], [[RHS]]) : $@convention(c) (@inout IntBox, IntBox) -> IntBox
// CHECK: end_access [[SELFACCESS]] : $*IntBox

// CHECK: sil [clang IntBox."+"] [[NAME]] : $@convention(c) (@inout IntBox, IntBox) -> IntBox
