// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s
import ctypes

func testVoid() {
  var x : MyVoid // expected-error{{use of undeclared type 'MyVoid'}}
  returnsMyVoid()
}
