// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
//
// We can't yet call member functions correctly on Windows (SR-13129).
// XFAIL: OS=windows-msvc

import MemberOutOfLine

public func add(_ lhs: inout IntBox, _ rhs: IntBox) -> IntBox { lhs + rhs }

// CHECK: bb0([[LHS:%.*]] : $*IntBox, [[RHS:%.*]] : $IntBox):
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [static] [[LHS]] : $*IntBox
// CHECK:   [[FUNC:%.*]] = function_ref [[NAME:@_ZNK6IntBoxplES_]] : $@convention(c) (@inout IntBox, IntBox) -> IntBox
// CHECK:   apply [[FUNC]]([[ACCESS]], [[RHS]]) : $@convention(c) (@inout IntBox, IntBox) -> IntBox
// CHECK:   end_access [[ACCESS]] : $*IntBox

// CHECK: sil [clang IntBox."+"] [[NAME]] : $@convention(c) (@inout IntBox, IntBox) -> IntBox
