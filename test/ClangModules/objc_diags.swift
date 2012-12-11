// RUN: rm -rf %t/clang-module-cache
// RUN: not %swift -parse  -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s 2> %t.out
// RUN: FileCheck %s < %t.out
import objc

func instanceMethod(b : B) {
  // CHECK: error: no candidates found for call
  // CHECK: func [objc] method(i : CInt, onExtB : CDouble) -> id
  // CHECK: func [objc] method(arg : CInt, withFloat : CFloat) -> CInt
  // CHECK: func [objc] method(i : CInt, onExtA : CDouble) -> id
  // CHECK: func [objc] method(i : CInt, onCat1 : CDouble) -> id
  // CHECK: func [objc] method(arg : CInt, withDouble : CDouble) -> CInt
  b.method(1, 2.5)
}
