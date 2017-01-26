// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-ir -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

class A {
  var a : A?
  // CHECK: define {{.*}}1ACACycfc
  init() {
    // This store that sets up the stack slot should be on line 0 so
    // the debugger may skip ahead.
    // CHECK:   store {{.*}} %0, {{.*}}, !dbg ![[DBG:[0-9]+]]
    // CHECK: ![[DBG]] = {{.*}}line: 0
    markUsed("Hi")
  }
}

let a = A()
