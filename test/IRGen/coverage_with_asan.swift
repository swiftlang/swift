// RUN: %target-swift-frontend %s -profile-generate -profile-coverage-mapping -sanitize=address -emit-ir -o - | FileCheck %s

// CHECK: main
func main() {}
