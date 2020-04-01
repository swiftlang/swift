// RUN: %target-swift-frontend %s -profile-generate -profile-coverage-mapping -sanitize=address -emit-ir -o - | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// REQUIRES: OS=macosx

// CHECK: main
func main() {}
