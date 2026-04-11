// RUN: %target-swift-frontend -primary-file %s -emit-ir -O -swift-version 5 -plugin-path %swift-plugin-dir -module-name main | %FileCheck %s

// REQUIRES: concurrency

@TaskLocal var taskLocal: (() -> Void)?

// Verify that taskLocalValuePush uses the formal AST type metadata (Optional<() -> ()>, mangled as "yycSg"):
// CHECK-LABEL: define {{.*}}swiftcc void @"$s4main4testyyF"()
// CHECK: call ptr @__swift_instantiateConcreteTypeFromMangledName{{.*}}@"$syycSg
// CHECK: call swiftcc {} @swift_task_localValuePush(
func test() {
  $taskLocal.withValue({}) {
    print("inside withValue")
  }
}
