// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo | %FileCheck %s

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
