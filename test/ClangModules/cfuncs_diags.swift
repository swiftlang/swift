// RUN: rm -rf %t/clang-module-cache
// RUN: not %swift -parse  -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s 2> %t.out
// RUN: FileCheck %s < %t.out
import cfuncs

func exit(_ : Float) {}

func test_exit() {
  // CHECK: no candidates found for call
  // CHECK: found this candidate
  // CHECK: func exit(_ : Float) {}
  // CHECK: cfuncs.exit:1:6: note: found this candidate
  // CHECK: func exit(_ : CInt)
  exit(5)
}
