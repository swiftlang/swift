// RUN: %target-swift-frontend -emit-ir -profile-generate -sanitize=thread %s | %FileCheck %s

// rdar://43422035 unsupported option for target i386-apple-ios
// XFAIL: CPU=i386

// CHECK: define {{.*}}empty
// CHECK-NOT: load{{.*}}empty
// CHECK: ret void
func empty() {
}

empty()
