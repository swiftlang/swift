// RUN: %target-swift-frontend %s -Onone -g -emit-ir -o - | FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// Check that the mandatiry constant propagation preserves
// debug_info even if the value related to it is not used
// by any other instruction besides debug_info.

// CHECK: !DILocalVariable(tag: DW_TAG_auto_variable, name: "y"
// CHECK: !DILocalVariable(tag: DW_TAG_auto_variable, name: "x"
func main() {
  let x = 10
  var y = 10
  print(x+y, appendNewline: true) // Set breakpoint here
}

main()

