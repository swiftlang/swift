// REQUIRES: tsan_runtime
// RUN: %target-swift-frontend -emit-ir -profile-generate -sanitize=thread %s | %FileCheck %s

// TSan is only supported on 64 bit.
// REQUIRES: PTRSIZE=64

// CHECK: define {{.*}}empty
// CHECK-NOT: load{{.*}}empty
// CHECK: ret void
func empty() {
}

empty()
