// RUN: %target-swift-frontend -Ounchecked -emit-ir -primary-file %s | FileCheck %s

// CHECK-LABEL: define {{.*}}@{{.*}}test_aliasing{{.*}}({{.*}}noalias{{.*}})
func test_aliasing(inout a: [Int]) {
}
