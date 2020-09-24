// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
//
// We can't yet call member functions correctly on Windows (SR-13129).
// XFAIL: OS=windows-msvc

import MemberInline

public func sub(_ lhs: inout IntBox, _ rhs: IntBox) -> IntBox { lhs - rhs }

// CHECK: call [[RES:i32|i64]] [[NAME:@(_ZN6IntBoxmiES_|"\?\?GIntBox@@QEAA\?AU0@U0@@Z")]](%struct.IntBox* {{%[0-9]+}}, {{i32|\[1 x i32\]|i64|%struct.IntBox\* byval align 4}} {{%[0-9]+}})
// CHECK: define linkonce_odr [[RES]] [[NAME]](%struct.IntBox* %this, {{i32 %rhs.coerce|\[1 x i32\] %rhs.coerce|i64 %rhs.coerce|%struct.IntBox\* byval\(%struct.IntBox\) align 4 %rhs}})
