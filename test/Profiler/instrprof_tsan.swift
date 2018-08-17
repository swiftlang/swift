// RUN: %target-swift-frontend -emit-ir -profile-generate -sanitize=thread %s | %FileCheck %s

// CHECK: define {{.*}}empty
// CHECK-NOT: load{{.*}}empty
// CHECK: ret void
func empty() {
}

empty()
