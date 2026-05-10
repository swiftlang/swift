// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo | %FileCheck %s


// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

public class MyClass {}

public func foo() -> MyClass {
  return MyClass()
}

public class MySubClass: MyClass {}


public func bar() -> MyClass {
  return MySubClass()
}


// CHECK-DAG: @"$e4main10MySubClassCMf" = {{.*}} <{ ptr @"$eBoWV", ptr getelementptr inbounds ({{.*}}, ptr @"$e4main7MyClassCMf", i32 0, i32 1), ptr @"$e4main10MySubClassCfD", ptr null, ptr @"$e4main10MySubClassCACycfC" }>
// CHECK-DAG: @"$e4main7MyClassCMf" = {{.*}} <{ ptr @"$eBoWV", ptr null, ptr @"$e4main7MyClassCfD", ptr null, ptr @"$e4main7MyClassCACycfC" }>

// CHECK-DAG: @"$e4main10MySubClassCN" = {{.*}} ptr @"$e4main10MySubClassCMf", i32 0, i32 1)
// CHECK-DAG: @"$e4main7MyClassCN" = {{.*}} ptr @"$e4main7MyClassCMf", i32 0, i32 1)

// CHECK-DAG: define {{.*}}ptr @"$e4main3barAA7MyClassCyF"


// CHECK-DAG: define {{.*}}ptr @"$e4main7MyClassCfd"
// CHECK-DAG: define {{.*}}void @"$e4main7MyClassCfD"
// CHECK-DAG: define {{.*}}ptr @"$e4main7MyClassCACycfC"
// CHECK-DAG: define {{.*}}ptr @"$e4main7MyClassCACycfc"


// CHECK-DAG: define {{.*}}ptr @"$e4main3fooAA7MyClassCyF"


// CHECK-DAG: define {{.*}}ptr @"$e4main10MySubClassCACycfC"
// CHECK-DAG: define {{.*}}ptr @"$e4main10MySubClassCACycfc"
// CHECK-DAG: define {{.*}}ptr @"$e4main10MySubClassCfd"
// CHECK-DAG: define {{.*}}void @"$e4main10MySubClassCfD"
