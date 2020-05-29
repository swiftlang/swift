// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import NonMemberOutOfLine

public func add(_ lhs: IntBox, _ rhs: IntBox) -> IntBox { lhs + rhs }

// CHECK: call i32 @_Zpl6IntBoxS_(i32 %{{[0-9]+}}, i32 %{{[0-9]+}})
// CHECK: declare i32 @_Zpl6IntBoxS_(i32, i32)
