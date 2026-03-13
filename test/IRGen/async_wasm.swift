// RUN: %target-swift-frontend -primary-file %S/async.swift -enable-builtin-module -emit-ir  -disable-availability-checking | %FileCheck %s --check-prefix=NOTAIL
// TODO(katei): %target-swift-frontend -primary-file %S/async.swift -enable-builtin-module -emit-ir  -disable-availability-checking -Xcc -mtail-call | %FileCheck %s --check-prefix=TAIL

// REQUIRES: concurrency && CPU=wasm32

// NOTAIL: "$s5async1fyyYaF"
// NOTAIL: "$s5async1gyyYaKF"
// NOTAIL: "$s5async1hyyS2iYbXEF"

// NOTAIL: define{{.*}} swiftcc void @"$s5async8testThisyyBonYaF"(ptr swiftasync %0{{.*}}
// NOTAIL-NOT: @swift_task_alloc
// NOTAIL-NOT: musttail call
// NOTAIL: call swiftcc void @swift_task_future_wait_throwing(ptr {{.*}}, ptr {{.*}}, ptr {{.*}}, ptr {{.*}}, ptr {{.*}})
