// RUN: %target-swift-emit-ir -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none | %FileCheck %s

// FIXME: enable on Windows
// XFAIL: OS=windows-msvc

import VirtualMethods

var x = DerivedInt()
x.callMe()

var b3 = Base3()
var d2 = Derived2()
var d3 = Derived3()
var d4 = Derived4()

b3.f()
d2.f()
d3.f()
d4.f()

// CHECK: invoke {{.*}} @_ZN5Base31fEv
// CHECK: invoke {{.*}} @_ZN8Derived21fEv
// CHECK: invoke {{.*}} @_ZN8Derived31fEv
// CHECK: call swiftcc {{.*}} @"$sSo8Derived4V1fs5Int32VyF"

// CHECK: define {{.*}} @"$sSo8Derived4V1fs5Int32VyF"(ptr swiftself dereferenceable
// CHECK: invoke {{.*}}  @_ZN8Derived423__synthesizedBaseCall_fEv

// CHECK: define {{.*}}void @{{_ZN7DerivedIiE3fooEv|"\?foo@\?$Derived@H@@UEAAXXZ"}}
// CHECK:   call void @{{_Z21testFunctionCollectedv|"\?testFunctionCollected@@YAXXZ"}}

// CHECK: define {{.*}}void @{{_Z21testFunctionCollectedv|"\?testFunctionCollected@@YAXXZ"}}

// CHECK-NOT: _ZN6UnusedIiE3fooEv
// CHECK-NOT: "\?foo@\?$Unused@H@@UEAAXXZ"
