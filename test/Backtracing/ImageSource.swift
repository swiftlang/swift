// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library \
// RUN:   -o %t/ImageSource
// RUN: %target-codesign %t/ImageSource
// RUN: (%target-run %t/ImageSource 2>&1 || true) | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu

@_spi(ImageSourceTest) import Runtime

@main
struct ImageSourceTest {
  static func main() {
    // CHECK: count: 0
    // CHECK-NEXT: bytes.count: 123
    // CHECK-NEXT: mutableBytes.count: 0
    // CHECK-NEXT: unusedBytes.count: 123

    // CHECK: reserve 120 bytes
    // CHECK-NEXT: count: 120
    // CHECK-NEXT: bytes.count: 123
    // CHECK-NEXT: mutableBytes.count: 120
    // CHECK-NEXT: unusedBytes.count: 3

    // CHECK: reserve 3 bytes
    // CHECK-NEXT: count: 123
    // CHECK-NEXT: bytes.count: 123
    // CHECK-NEXT: mutableBytes.count: 123
    // CHECK-NEXT: unusedBytes.count: 0

    // CHECK: reserve 1 byte
    // CHECK: {{.*}}ImageSource.swift{{.*}}: Fatal error: Buffer overrun detected
    if !testImageSource() {
      print("ImageSource tests failed")
    }
  }
}
