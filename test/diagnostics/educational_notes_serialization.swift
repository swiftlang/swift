// not %target-swift-frontend -no-color-diagnostics -print-educational-notes -diagnostic-documentation-path %S/test-docs/ -typecheck %s

// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck -no-color-diagnostics -print-educational-notes -diagnostic-documentation-path %S/test-docs/ -serialize-diagnostics-path %t/serialized.dia %s
// RUN: c-index-test -read-diagnostics %t/serialized.dia > %t/serialized.txt 2>&1
// RUN: %FileCheck %s < %t/serialized.txt

typealias Fn = () -> ()
extension Fn {}
// CHECK: [[@LINE-1]]:1: error: non-nominal type 'Fn' (aka '() -> ()') cannot be extended [{{.*}}nominal-types] [NominalTypes]


// Shares the flag record with `Fn`
typealias Dup = () -> ()
extension Dup {}
// CHECK: [[@LINE-1]]:1: error: non-nominal type 'Dup' (aka '() -> ()') cannot be extended [{{.*}}nominal-types] [NominalTypes]

do {
  func noNote(_: Int) {}
  noNote("Hello")
  // CHECK: [[@LINE-1]]:10: error: cannot convert value of type 'String' to expected argument type 'Int' [] []
}
