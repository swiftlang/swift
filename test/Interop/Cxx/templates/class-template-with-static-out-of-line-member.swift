// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop -disable-availability-checking | %FileCheck %s

import ClassTemplateWithOutOfLineMember

public func usesStaticMember() {
  HasStaticOutOfLineMemberInt.getFirstValue()
}

// CHECK: @{{_ZN24HasStaticOutOfLineMemberIiE6valuesE|"\?values@\?\$HasStaticOutOfLineMember@H@@2PAHA"}} = linkonce_odr{{.*}} global {{.*}} zeroinitializer

// CHECK: define {{.*}}i32 @{{_ZN24HasStaticOutOfLineMemberIiE13getFirstValueEv|"\?getFirstValue@\?\$HasStaticOutOfLineMember@H@@SAHXZ"}}()
// CHECK: %0 = {{.*}} @{{_ZN24HasStaticOutOfLineMemberIiE6valuesE|"\?values@\?\$HasStaticOutOfLineMember@H@@2PAHA"}}
