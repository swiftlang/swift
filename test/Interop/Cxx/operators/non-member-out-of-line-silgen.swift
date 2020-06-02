// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import NonMemberOutOfLine

public func add(_ lhs: IntBox, _ rhs: IntBox) -> IntBox { lhs + rhs }

// CHECK: [[COUNTER:%.*]] = function_ref [[NAME:@.*]] : $@convention(c) (IntBox, IntBox) -> IntBox
// CHECK: apply [[COUNTER]](%0, %1) : $@convention(c) (IntBox, IntBox) -> IntBox

// CHECK: sil [serializable] [clang "+"] [[NAME]] : $@convention(c) (IntBox, IntBox) -> IntBox
