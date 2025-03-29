// RUN: %target-swift-frontend -enable-copy-propagation=requested-passes-only -enable-lexical-lifetimes=false %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend -enable-copy-propagation=requested-passes-only -enable-lexical-lifetimes=false %s -Xllvm -sil-print-types -emit-sil -g -o - | %FileCheck -check-prefix=CHECK-SIL %s
import StdlibUnittest

// Test that debug info for local variables is preserved by the
// mandatory SIL optimization passes.

func main() {
  // CHECK-SIL-DAG: debug_value {{.*}}: $Int, let, name "x"
  // CHECK-DAG: DILocalVariable(name: "x"
  let x = 10
  // CHECK-SIL-DAG: alloc_stack [var_decl] $Int, var, name "y"
  // CHECK-DAG: DILocalVariable(name: "y"
  var y = 10
  // The expression x+y may become constant folded.
  _blackHole(x+y)
}

main()
