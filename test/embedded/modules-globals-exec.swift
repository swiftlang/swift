// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -enable-experimental-feature Extern -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -enable-experimental-feature Extern -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a.o
// RUN: %target-clang -x c -c %S/Inputs/print.c -o %t/print.o
// RUN: %target-clang %t/a.o %t/print.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

// BEGIN MyModule.swift

public var global_in_module_used_in_module = 0
public var global_in_module_unused_in_module = 0

public func foo() {
  global_in_module_used_in_module += 1
}

// BEGIN Main.swift

import MyModule

@_extern(c, "putchar")
@discardableResult
func putchar(_: CInt) -> CInt

public func print(_ s: StaticString, terminator: StaticString = "\n") {
  var p = s.utf8Start
  while p.pointee != 0 {
    putchar(CInt(p.pointee))
    p += 1
  }
  p = terminator.utf8Start
  while p.pointee != 0 {
    putchar(CInt(p.pointee))
    p += 1
  }
}

@_silgen_name("print_long")
func print_long(_: Int)

public func print(_ n: Int, terminator: StaticString = "\n") {
    print_long(n)
    print("", terminator: terminator)
}

func test() {
  print("Testing globals...") // CHECK: Testing globals...
  print(global_in_module_used_in_module) // CHECK-NEXT: 0
  print(global_in_module_unused_in_module) // CHECK-NEXT: 0
  foo()
  print(global_in_module_used_in_module) // CHECK-NEXT: 1
  print(global_in_module_unused_in_module) // CHECK-NEXT: 0
}

test()
