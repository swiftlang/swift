// RUN: %target-swift-frontend -print-ast %s 2>&1 | %FileCheck %s

// CHECK-LABEL: func test() {
func test() {
  log(1)
  log(1.0)
  log(true)
  log([1, 2, 3])
  log([1: true, 2: false])
}
// CHECK: log(1)
// CHECK: log(1.0)
// CHECK: log(true)
// CHECK: log([1, 2, 3])
// CHECK: log([1: true, 2: false])

func log(_ a: Any) {
}
