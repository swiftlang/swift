// RUN: %target-swift-frontend %s -profile-generate -profile-coverage-mapping -sanitize=address -emit-ir -o - | %FileCheck %s
// REQUIRES: OS=macosx

// CHECK: main
func main() {}
