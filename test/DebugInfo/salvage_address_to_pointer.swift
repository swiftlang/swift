// RUN: %target-swift-frontend -emit-sil -O -g %s | %FileCheck --check-prefix=CHECK-SIL %s
// RUN: %target-swift-frontend -emit-ir -O -g %s | %FileCheck --check-prefix=CHECK-IR %s

// Verify that the closure argument ($0) is salvaged via a debug reconstruction
// block containing address_to_pointer when the closure is inlined.

// CHECK-SIL-LABEL: sil @$s{{.*}}5hello
// CHECK-SIL: debug_value %{{[0-9]+}}, let, name "$0", argno 1, type $UnsafePointer<Int>, expr op_fragment:#UnsafePointer._rawValue, transform {
// CHECK-SIL:   bb0(%[[ARG:[0-9]+]] : $*Int):
// CHECK-SIL:     %[[PTR:[0-9]+]] = address_to_pointer [stack_protection] %[[ARG]]
// CHECK-SIL:     return %[[PTR]]
// CHECK-SIL:   }

// CHECK-IR-LABEL: define {{.*}}hello
// CHECK-IR: #dbg_value(ptr @"$s{{.*}}1iSivp", ![[VAR:[0-9]+]], !DIExpression()
// CHECK-IR: ![[VAR]] = !DILocalVariable(name: "$0"

var i: Int = 0

public func hello() {
  withUnsafePointer(to: &i) {
    print($0.pointee)
  }
}
