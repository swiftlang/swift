// RUN: %target-swift-emit-ir -Onone %s -I %S/Inputs -cxx-interoperability-mode=default \
// RUN:     -Xcc -fignore-exceptions -disable-availability-checking \
// RUN: | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc
// Windows/MSVC mangles symbols differently, and supporting those will only make
// this test more difficult to read and maintain. For now, the main prupose
// of this test is to ensure the correct retain/release functions are being
// selected, so should suffice to only test on non-Windows platforms.

import RefKit

// CHECK: define {{.*}} void @"$s4main8doObjectyyF"() #{{.*}} {
func doObject() {
  let o = RefObject.createNull()
  // CHECK: call void @_ZN3RefI6Object19RetainReleaseTraitsIS0_EE10createNullEv(ptr noalias sret(%{{.*}}) %[[o:.*]])

  let _: Object? = o.getPtrUnretained()
  // CHECK: %{{.*}} = call ptr @_ZNK3RefI6Object19RetainReleaseTraitsIS0_EE16getPtrUnretainedEv(ptr %[[o]])
  // CHECK: call void @_Z9refObjectP6Object(ptr %{{.*}})
  // CHECK: call void @_Z11derefObjectP6Object(ptr %{{.*}})

  let _: Object = o.getRefUnretained()
  // CHECK: %{{.*}} = call ptr @_ZNK3RefI6Object19RetainReleaseTraitsIS0_EE16getRefUnretainedEv(ptr %[[o]])
  // CHECK: call void @_Z9refObjectP6Object(ptr %{{.*}})
  // CHECK: call void @_Z11derefObjectP6Object(ptr %{{.*}})
}
// CHECK: }

// CHECK: define {{.*}} void @"$s4main11doNumberObjyyF"() #{{.*}} {
func doNumberObj() {
  let n = RefNumberObj.create(42.0)
  // CHECK: call void @_ZN3RefI9NumberObj19RetainReleaseTraitsIS0_EE6createEf(ptr noalias sret(%{{.*}}) %[[n:.*]], float 4.200000e+01)

  let _: NumberObj? = n.getPtrUnretained()
  // CHECK: %{{.*}} = call ptr @_ZNK3RefI9NumberObj19RetainReleaseTraitsIS0_EE16getPtrUnretainedEv(ptr %[[n]])
  // CHECK: call void @_Z12refNumberObjP9NumberObj(ptr %{{.*}})
  // CHECK: call void @_Z14derefNumberObjP9NumberObj(ptr %{{.*}})

  let _: NumberObj = n.getRefUnretained()
  // CHECK: %{{.*}} = call ptr @_ZNK3RefI9NumberObj19RetainReleaseTraitsIS0_EE16getRefUnretainedEv(ptr %[[n]])
  // CHECK: call void @_Z12refNumberObjP9NumberObj(ptr %{{.*}})
  // CHECK: call void @_Z14derefNumberObjP9NumberObj(ptr %{{.*}})
}
// CHECK: }

// CHECK: define {{.*}} void @"$s4main12doBoxedFloatyyF"() #0 {
func doBoxedFloat() {
  let f = RefBoxedFloat.create(42.0)
  // CHECK: call void @_ZN3RefI5BoxedIfE19RetainReleaseTraitsIS1_EE6createEf(ptr noalias sret(%{{.*}}) %[[f:.*]], float 4.200000e+01)

  let _: BoxedFloat? = f.getPtrUnretained()
  // CHECK: %{{.*}} = call ptr @_ZNK3RefI5BoxedIfE19RetainReleaseTraitsIS1_EE16getPtrUnretainedEv(ptr %[[f]])
  // CHECK: call void @_ZNK5BoxedIfE3refEv(ptr %{{.*}})
  // CHECK: call void @_ZNK5BoxedIfE5derefEv(ptr {{.*}})

  let _: BoxedFloat = f.getRefUnretained()
  // CHECK: %{{.*}} = call ptr @_ZNK3RefI5BoxedIfE19RetainReleaseTraitsIS1_EE16getRefUnretainedEv(ptr %[[f]])
  // CHECK: call void @_ZNK5BoxedIfE3refEv(ptr %{{.*}})
  // CHECK: call void @_ZNK5BoxedIfE5derefEv(ptr {{.*}})
}
// CHECK: }

