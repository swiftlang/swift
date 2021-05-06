// RUN: %swift -I %S/Inputs -enable-cxx-interop -enable-objc-interop -emit-ir %s | %FileCheck %s

import AnonymousUnionPartlyInvalid

let sPtr = getSPtr()
let a = sPtr![0].f()

// CHECK: i32 @main
// CHECK-NEXT: entry:
// CHECK-NEXT: bitcast
// CHECK-NEXT: call %struct.S
// CHECK-NEXT: ptrtoint %struct.S

