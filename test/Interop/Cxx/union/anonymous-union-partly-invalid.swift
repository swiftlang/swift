// RUN: %swift -I %S/Inputs -enable-experimental-cxx-interop -enable-objc-interop -emit-ir %s -Xcc -fignore-exceptions | %FileCheck %s

import AnonymousUnionPartlyInvalid

let sPtr = getSPtr()
let a = sPtr![0].f()

// CHECK: i32 @main
// CHECK-NEXT: entry:
// CHECK-NEXT: bitcast
// CHECK-NEXT: call %struct.S
// CHECK-NEXT: ptrtoint %struct.S

