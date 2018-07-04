// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -profile-generate -profile-coverage-mapping -sanitize=address -emit-ir -o - | %FileCheck %s
// REQUIRES: OS=macosx

// CHECK: main
func main() {}
