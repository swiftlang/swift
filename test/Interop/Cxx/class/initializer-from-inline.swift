// RUN: %swift -I %S/Inputs -enable-cxx-interop -emit-ir %s | %FileCheck %s

import InitializerFromInline

// CHECK: define linkonce_odr i32 @{{_Z5get42v|\?get42@@YAHXZ}}
// CHECK: define linkonce_odr i32 @{{_Z15passThroughArgTIiET_S0_|\?\?$passThroughArgT@H@@YAHH@Z}}
// CHECK: define linkonce_odr i32 @{{_Z11get42Level1v|\?get42Level1@@YAHXZ}}
// CHECK: define linkonce_odr i32 @{{_Z11get42Level2v|\?get42Level2@@YAHXZ}}
// CHECK: define linkonce_odr i32 @{{_Z11get42Level3v|\?get42Level3@@YAHXZ}}
// CHECK: define linkonce_odr i32 @{{_Z11get42Level4v|\?get42Level4@@YAHXZ}}

var a = Hold42()
var b = Hold23()
var c = HoldMemberThatHolds42()
var d = HoldMemberThatHoldsMemberThatHolds42()
var e = Hold42WithLongInitCallGraph()

let sum = a.m + b.m + c.m.m + d.m.m.m + e.m

print("Sum: \(sum)")