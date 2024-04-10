// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo | %FileCheck %s

// REQUIRES: swift_in_compiler

public class MyClass {}

// CHECK-DAG: @"$s4main7MyClassCN" = {{.*}}<{ ptr, ptr, ptr, ptr }> <{ ptr null, ptr @"$s4main7MyClassCfD", ptr null, ptr @"$s4main7MyClassCACycfC" }>
// CHECK-DAG: define {{.*}}ptr @"$s4main7MyClassCfd"
// CHECK-DAG: define {{.*}}void @"$s4main7MyClassCfD"
// CHECK-DAG: define {{.*}}ptr @"$s4main7MyClassCACycfC"
// CHECK-DAG: define {{.*}}ptr @"$s4main7MyClassCACycfc"

public func foo() -> MyClass {
  return MyClass()
}
// CHECK-DAG: define {{.*}}ptr @"$s4main3fooAA7MyClassCyF"

public class MySubClass: MyClass {}

// CHECK-DAG: @"$s4main10MySubClassCN" = {{.*}}<{ ptr, ptr, ptr, ptr }> <{ ptr @"$s4main7MyClassCN", ptr @"$s4main10MySubClassCfD", ptr null, ptr @"$s4main10MySubClassCACycfC" }>
// CHECK-DAG: define {{.*}}ptr @"$s4main10MySubClassCACycfC"
// CHECK-DAG: define {{.*}}ptr @"$s4main10MySubClassCACycfc"
// CHECK-DAG: define {{.*}}ptr @"$s4main10MySubClassCfd"
// CHECK-DAG: define {{.*}}void @"$s4main10MySubClassCfD"

public func bar() -> MyClass {
  return MySubClass()
}
// CHECK-DAG: define {{.*}}ptr @"$s4main3barAA7MyClassCyF"
