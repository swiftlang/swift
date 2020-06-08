// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import MemberInline

public func add(_ lhs: inout IntBox, _ rhs: IntBox) -> IntBox { lhs + rhs }

// CHECK: call i32 @_ZN6IntBoxplES_(%struct.IntBox* %3, i32 %1)
// CHECK: define linkonce_odr i32 @_ZN6IntBoxplES_(%struct.IntBox* %this, i32 %rhs.coerce) #2 comdat align 2 {
