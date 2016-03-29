// REQUIRES: CPU=x86_64
// RUN: %target-swift-frontend -disable-red-zone -emit-ir  %s | FileCheck --check-prefix=CHECK %s


func test1() {
}

// CHECK: ; Function Attrs: noredzone
// CHECK: attributes #{{[0-9]+}} = { noredzone
