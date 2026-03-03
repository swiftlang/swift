// RUN: %target-swift-emit-irgen %s -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -fignore-exceptions -disable-availability-checking | %FileCheck %s

import PassAsParameter

public func refToPtr() {
  var a = IntBox.create(123)
  let aValue = extractValueFromRefToPtr(&a)
  print(aValue)
}
// CHECK: define{{.*}} void {{.*}}refToPtr{{.*}}()
// CHECK:   [[PTR_TO_PTR_TO_INT_BOX:%.*]] = alloca %TSo6IntBoxVSg
// CHECK:   {{.*}} = call {{.*}} @{{.*}}extractValueFromRefToPtr{{.*}}(ptr [[PTR_TO_PTR_TO_INT_BOX]])

public func constRefToPtr() {
  let a = IntBox.create(456)
  let aValue = extractValueFromConstRefToPtr(a)
  print(aValue)
}
// CHECK: define{{.*}} void {{.*}}constRefToPtr{{.*}}()
// CHECK:   [[PTR_TO_PTR_TO_INT_BOX2:%.*]] = alloca %TSo6IntBoxVSg
// CHECK:   {{.*}} = call {{.*}} @{{.*}}extractValueFromConstRefToPtr{{.*}}(ptr [[PTR_TO_PTR_TO_INT_BOX2]])

public func refToConstPtr() {
  var a = IntBox.create(321)
  let aValue = extractValueFromRefToConstPtr(&a)
  print(aValue)
}
// CHECK: define{{.*}} void {{.*}}refToConstPtr{{.*}}()
// CHECK:   [[PTR_TO_PTR_TO_INT_BOX3:%.*]] = alloca %TSo6IntBoxVSg
// CHECK:   {{.*}} = call {{.*}} @{{.*}}extractValueFromRefToConstPtr{{.*}}(ptr [[PTR_TO_PTR_TO_INT_BOX3]])

public func constRefToConstPtr() {
  let a = IntBox.create(789)
  let aValue = extractValueFromConstRefToConstPtr(a)
  print(aValue)
}
// CHECK: define{{.*}} void {{.*}}constRefToConstPtr{{.*}}()
// CHECK:   [[PTR_TO_PTR_TO_INT_BOX4:%.*]] = alloca %TSo6IntBoxVSg
// CHECK:   {{.*}} = call {{.*}} @{{.*}}extractValueFromConstRefToConstPtr{{.*}}(ptr [[PTR_TO_PTR_TO_INT_BOX4]])
