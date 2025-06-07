// RUN: %target-swift-emit-ir -I %S/Inputs -cxx-interoperability-mode=upcoming-swift %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s
// RUN: %target-swift-emit-ir -I %S/Inputs -cxx-interoperability-mode=swift-5.9 %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s
// RUN: %target-swift-emit-ir -I %S/Inputs -cxx-interoperability-mode=swift-6 %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

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

// CHECK: call {{.*}} @{{_ZN5Base31fEv|"\?f@Base3@@UEAAHXZ"}}
// CHECK: call {{.*}} @{{_ZN8Derived21fEv|"\?f@Derived2@@UEAAHXZ"}}
// CHECK: call {{.*}} @{{_ZN8Derived31fEv|"\?f@Derived3@@UEAAHXZ"}}
// CHECK: call swiftcc {{.*}} @"$sSo8Derived4V1fs5Int32VyF"

// CHECK: define {{.*}} @"$sSo8Derived4V1fs5Int32VyF"(ptr swiftself dereferenceable
// CHECK: call {{.*}}  @{{_ZN8Derived423__synthesizedBaseCall_fEv|"\?__synthesizedBaseCall_f@Derived4@@QEAAHXZ"}}

// CHECK: define {{.*}}void @{{_ZN7DerivedIiE3fooEv|"\?foo@\?\$Derived@H@@UEAAXXZ"}}
// CHECK:   call void @{{_Z21testFunctionCollectedv|"\?testFunctionCollected@@YAXXZ"}}

// CHECK: define {{.*}}void @{{_Z21testFunctionCollectedv|"\?testFunctionCollected@@YAXXZ"}}

// CHECK-NOT: _ZN6UnusedIiE3fooEv
// CHECK-NOT: "\?foo@\?\$Unused@H@@UEAAXXZ"
