// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo -disable-experimental-feature EmbeddedExistentials | %FileCheck %s
// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo | %FileCheck %s --check-prefix=EXIST


// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

public class MyClass {}

// CHECK-DAG: @"$e4main7MyClassCN" = {{.*}}<{ ptr, ptr, ptr, ptr }> <{ ptr null, ptr @"$e4main7MyClassCfD", ptr null, ptr @"$e4main7MyClassCACycfC" }>
// CHECK-DAG: define {{.*}}ptr @"$e4main7MyClassCfd"
// CHECK-DAG: define {{.*}}void @"$e4main7MyClassCfD"
// CHECK-DAG: define {{.*}}ptr @"$e4main7MyClassCACycfC"
// CHECK-DAG: define {{.*}}ptr @"$e4main7MyClassCACycfc"

public func foo() -> MyClass {
  return MyClass()
}
// CHECK-DAG: define {{.*}}ptr @"$e4main3fooAA7MyClassCyF"

public class MySubClass: MyClass {}

// CHECK-DAG: @"$e4main10MySubClassCN" = {{.*}}<{ ptr, ptr, ptr, ptr }> <{ ptr @"$e4main7MyClassCN", ptr @"$e4main10MySubClassCfD", ptr null, ptr @"$e4main10MySubClassCACycfC" }>
// CHECK-DAG: define {{.*}}ptr @"$e4main10MySubClassCACycfC"
// CHECK-DAG: define {{.*}}ptr @"$e4main10MySubClassCACycfc"
// CHECK-DAG: define {{.*}}ptr @"$e4main10MySubClassCfd"
// CHECK-DAG: define {{.*}}void @"$e4main10MySubClassCfD"

public func bar() -> MyClass {
  return MySubClass()
}
// CHECK-DAG: define {{.*}}ptr @"$e4main3barAA7MyClassCyF"


// EXIST-DAG: @"$e4main10MySubClassCMf" = {{.*}} <{ ptr @"$eBoWV", ptr getelementptr inbounds ({{.*}}, ptr @"$e4main7MyClassCMf", i32 0, i32 1), ptr @"$e4main10MySubClassCfD", ptr null, ptr @"$e4main10MySubClassCACycfC" }>
// EXIST-DAG: @"$e4main7MyClassCMf" = {{.*}} <{ ptr @"$eBoWV", ptr null, ptr @"$e4main7MyClassCfD", ptr null, ptr @"$e4main7MyClassCACycfC" }>

// EXIST-DAG: @"$e4main10MySubClassCN" = {{.*}} ptr @"$e4main10MySubClassCMf", i32 0, i32 1)
// EXIST-DAG: @"$e4main7MyClassCN" = {{.*}} ptr @"$e4main7MyClassCMf", i32 0, i32 1)

// EXIST-DAG: define {{.*}}ptr @"$e4main3barAA7MyClassCyF"


// EXIST-DAG: define {{.*}}ptr @"$e4main7MyClassCfd"
// EXIST-DAG: define {{.*}}void @"$e4main7MyClassCfD"
// EXIST-DAG: define {{.*}}ptr @"$e4main7MyClassCACycfC"
// EXIST-DAG: define {{.*}}ptr @"$e4main7MyClassCACycfc"


// EXIST-DAG: define {{.*}}ptr @"$e4main3fooAA7MyClassCyF"


// EXIST-DAG: define {{.*}}ptr @"$e4main10MySubClassCACycfC"
// EXIST-DAG: define {{.*}}ptr @"$e4main10MySubClassCACycfc"
// EXIST-DAG: define {{.*}}ptr @"$e4main10MySubClassCfd"
// EXIST-DAG: define {{.*}}void @"$e4main10MySubClassCfD"
