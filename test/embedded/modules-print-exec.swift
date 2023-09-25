// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a.o
// RUN: %target-clang -x c -c %S/Inputs/print.c -o %t/print.o
// RUN: %target-clang %t/a.o %t/print.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

// BEGIN MyModule.swift

@_silgen_name("putchar")
func putchar(_: UInt8)

public func print(_ s: StaticString, terminator: StaticString) {
  var p = s.utf8Start
  while p.pointee != 0 {
    putchar(p.pointee)
    p += 1
  }
  p = terminator.utf8Start
  while p.pointee != 0 {
    putchar(p.pointee)
    p += 1
  }
}

@_silgen_name("print_long")
func print_long(_: Int)

public func print(_ n: Int, terminator: StaticString) {
    print_long(n)
    print("", terminator: terminator)
}

// BEGIN Main.swift

import MyModule

func test() {
  print("Hello world", terminator: "\n") // CHECK: Hello world
  print(42, terminator: "\n") // CHECK-NEXT: 42
}

test()
