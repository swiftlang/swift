// RUN: %target-swift-frontend -primary-file %s -emit-ir -O -swift-version 5 -plugin-path %swift-plugin-dir -module-name main | %FileCheck %s

// REQUIRES: concurrency

@TaskLocal var taskLocal: (() -> Void)?

// Verify that taskLocalValuePush uses the formal AST type metadata (Optional<() -> ()>, mangled as "yycSg"):
// CHECK-LABEL: define hidden swiftcc void @"$s4main4testyyF"()
// CHECK: %{{[0-9]+}} = {{(tail )?}}call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr nonnull @"$syycSgMD")
// CHECK: %{{[0-9]+}} = call swiftcc {} @swift_task_localValuePush(ptr %{{[0-9]+}}, ptr nonnull %{{[0-9]+}}, ptr %{{[0-9]+}})
func test() {
  $taskLocal.withValue({}) {
    print("inside withValue")
  }
}
