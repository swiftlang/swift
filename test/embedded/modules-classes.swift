// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift %S/Inputs/print.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a.o
// RUN: %target-clang -x c -c %S/Inputs/print.c -o %t/print.o
// RUN: %target-clang %t/a.o %t/print.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

// BEGIN MyModule.swift

internal class BaseClass {

}

final internal class MyClass: BaseClass {
  func foo() { print("MyClass.foo") }
}

public func foo() {
  let o = MyClass()
  o.foo()
}

// BEGIN Main.swift

import MyModule

func test() {
  foo()
}

test()

// CHECK: MyClass.foo
