// Test that Swift correctly dispatches to ObjC direct method thunks when
// -fobjc-direct-precondition-thunk is enabled.

// RUN: %target-swift-emit-ir -import-objc-header %S/../Inputs/objc_direct.h -Xcc -fobjc-direct-precondition-thunk -o - %s | %FileCheck %s

// REQUIRES: objc_interop

func markUsed<T>(_ t: T) {}

let bar = Bar()

// Instance method calls should go through the thunk.
bar.directProperty = 123
// CHECK: call void @"-[Bar setDirectProperty:]D_thunk"(ptr %{{[0-9]+}}, i32 {{.*}})

markUsed(bar.directProperty)
// CHECK: call i32 @"-[Bar directProperty]D_thunk"(ptr %{{[0-9]+}})

bar.directProperty2 = 456
// CHECK: call void @"-[Bar setDirectProperty2:]D_thunk"(ptr %{{[0-9]+}}, i32 {{.*}})

markUsed(bar.directProperty2)
// CHECK: call i32 @"-[Bar directProperty2]D_thunk"(ptr %{{[0-9]+}})

bar[0] = 789
// CHECK: call void @"-[Bar setObject:atIndexedSubscript:]D_thunk"(ptr %{{[0-9]+}}, i32 789, i32 0)

markUsed(bar[0])
// CHECK: call i32 @"-[Bar objectAtIndexedSubscript:]D_thunk"(ptr %{{[0-9]+}}, i32 0)

markUsed(bar.directMethod())
// CHECK: call {{.*}} @"-[Bar directMethod]D_thunk"(ptr %{{[0-9]+}})

markUsed(bar.directMethod2())
// CHECK: call {{.*}} @"-[Bar directMethod2]D_thunk"(ptr %{{[0-9]+}})

// Class method calls should also go through the thunk.
markUsed(Bar.directClassMethod())
// CHECK: call {{.*}} @"+[Bar directClassMethod]D_thunk"(ptr

markUsed(Bar.directClassMethod2())
// CHECK: call {{.*}} @"+[Bar directClassMethod2]D_thunk"(ptr

markUsed(bar.directProtocolMethod())
// CHECK: call {{.*}} @"-[Bar directProtocolMethod]D_thunk"({{.*}})

// Thunks should be declared with linkonce_odr hidden.
// CHECK-DAG: define linkonce_odr hidden {{.*}} @"-[Bar setDirectProperty:]D_thunk"
// CHECK-DAG: define linkonce_odr hidden {{.*}} @"-[Bar directProperty]D_thunk"
// CHECK-DAG: define linkonce_odr hidden {{.*}} @"-[Bar directMethod]D_thunk"
// CHECK-DAG: define linkonce_odr hidden {{.*}} @"+[Bar directClassMethod]D_thunk"

// True implementations should be declared (external, with D suffix).
// CHECK-DAG: declare {{.*}} @"-[Bar directProperty]D"
// CHECK-DAG: declare {{.*}} @"-[Bar setDirectProperty:]D"
// CHECK-DAG: declare {{.*}} @"-[Bar directMethod]D"
// CHECK-DAG: declare {{.*}} @"+[Bar directClassMethod]D"
