// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern %t/test.swift -c -o %t/a.o

// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.o | sort | tee %t/actual-dependencies.txt

// Fail if there is any entry in actual-dependencies.txt that's not in allowed-dependencies.txt
// RUN: %if OS=linux-gnu %{ comm -13 %t/allowed-dependencies_linux.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: %if OS=macosx %{ comm -13 %t/allowed-dependencies_macos.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: %if OS=wasip1 %{ comm -13 %t/allowed-dependencies_wasi.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: test ! -s %t/extra.txt

//--- allowed-dependencies_macos.txt
___stack_chk_fail
___stack_chk_guard
_arc4random_buf
_free
_memmove
_memset
_posix_memalign
_putchar

//--- allowed-dependencies_linux.txt
__stack_chk_fail
__stack_chk_guard
arc4random_buf
free
memmove
memset
posix_memalign
putchar
//--- allowed-dependencies_wasi.txt
__indirect_function_table
__memory_base
__stack_chk_fail
__stack_chk_guard
__stack_pointer
__table_base
arc4random_buf
free
posix_memalign
putchar
//--- test.swift
// RUN: %target-clang -x c -c %S/Inputs/print.c -o %t/print.o
// RUN: %target-clang -x c -c %S/Inputs/linux-rng-support.c -o %t/linux-rng-support.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/a.o %t/print.o %t/linux-rng-support.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// UNSUPPORTED: OS=linux-gnu && CPU=aarch64

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

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

class MyClass {
  func foo() { print("MyClass.foo") }
}

class MySubClass: MyClass {
  override func foo() { print("MySubClass.foo") }
}

@main
struct Main {
  static var objects: [MyClass] = []
  static func main() {
    print("Hello Embedded Swift!")
    // CHECK: Hello Embedded Swift!
    objects.append(MyClass())
    objects.append(MySubClass())
    for o in objects {
      o.foo()
    }
    // CHECK: MyClass.foo
    // CHECK: MySubClass.foo
    print(Bool.random() ? "you won" : "you lost")
    // CHECK: you {{won|lost}}
  }
}
