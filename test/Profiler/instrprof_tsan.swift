// RUN: %target-swift-frontend -emit-ir -profile-generate -sanitize=thread %s | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

// CHECK: define {{.*}}empty
// CHECK-NOT: load{{.*}}empty
// CHECK: ret void
func empty() {
}

empty()
