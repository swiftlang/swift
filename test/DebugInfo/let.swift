// RUN: %swift -triple x86_64-apple-darwin13 %s -emit-llvm -g -o - | FileCheck %s
func foo() -> Int { return 42}
func bar() -> Int {
// CHECK: [ DW_TAG_auto_variable ] [a] [line [[@LINE+1]]]
  let a = foo()
  foo()

  return a
}
