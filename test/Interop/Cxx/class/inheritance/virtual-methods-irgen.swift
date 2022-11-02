// RUN: %target-swift-emit-ir -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none | %FileCheck %s

// FIXME: enable on Windows
// XFAIL: OS=windows-msvc

import VirtualMethods

var x = DerivedInt()
x.callMe()

// CHECK: define {{.*}}void @{{_ZN7DerivedIiE3fooEv|"\?foo@\?$Derived@H@@UEAAXXZ"}}
// CHECK:   call void @{{_Z21testFunctionCollectedv|"\?testFunctionCollected@@YAXXZ"}}

// CHECK: define {{.*}}void @{{_Z21testFunctionCollectedv|"\?testFunctionCollected@@YAXXZ"}}

// CHECK-NOT: _ZN6UnusedIiE3fooEv
// CHECK-NOT: "\?foo@\?$Unused@H@@UEAAXXZ"
