// RUN: %target-swift-frontend %s -emit-irgen -g -o - \
// RUN:    -module-name M  -disable-availability-checking \
// RUN:    -parse-as-library | %FileCheck %s --check-prefix=CHECK

// REQUIRES: concurrency

// Check that all the helper "outlined" functions do not have debug information.

func ASYNC___1___() async -> Int {
  return 42
}

func ASYNC___2___() async -> Int {
  print("hello")
  var x = await ASYNC___1___()
  x += await ASYNC___1___()
  return x
}

// CHECK-LABEL: define {{.*}} @"$s1M12ASYNC___1___SiyYaF.0"
// CHECK-NOT: !dbg
// CHECK: ret void
// CHECK-NEXT: }

// CHECK-LABEL: define {{.*}} @__swift_async_resume_get_context
// CHECK-NOT: !dbg
// CHECK: ret ptr
// CHECK-NEXT: }

// CHECK-LABEL: define {{.*}} @"$s1M12ASYNC___2___SiyYaF.1"
// CHECK-NOT: !dbg
// CHECK: ret void
// CHECK-NEXT: }

// CHECK-LABEL: define {{.*}} @__swift_async_resume_project_context
// CHECK-NOT: !dbg
// CHECK: ret ptr
// CHECK-NEXT: }

// CHECK-LABEL: define {{.*}} @"$s1M12ASYNC___2___SiyYaF.0"
// CHECK-NOT: !dbg
// CHECK: ret void
// CHECK-NEXT: }

// CHECK-LABEL: define {{.*}} @"$s1M12ASYNC___2___SiyYaF.0.1"
// CHECK-NOT: !dbg
// CHECK: ret void
// CHECK-NEXT: }

// CHECK-LABEL: define {{.*}} @"$s1M12ASYNC___2___SiyYaF.0.2"
// CHECK-NOT: !dbg
// CHECK: ret void
// CHECK-NEXT: }
