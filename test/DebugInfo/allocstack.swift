// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s
// RUN: %target-swift-frontend %s -emit-sil -g -o - | FileCheck -check-prefix=CHECK-SIL %s
import StdlibUnittest

// Test that debug info for local variables is preserved by the
// mandatory SIL optimization passes.

func main() {
  // CHECK-SIL-DAG: debug_value {{.*}}let x
  // CHECK-DAG: DILocalVariable(tag: DW_TAG_auto_variable, name: "x"
  let x = 10
  // CHECK-SIL-DAG: alloc_stack {{.*}}var y
  // CHECK-DAG: DILocalVariable(tag: DW_TAG_auto_variable, name: "y"
  var y = 10
  // The expression x+y may become constant folded.
  _blackHole(x+y)
}

main()
