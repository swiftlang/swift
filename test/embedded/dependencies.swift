// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Extern -enable-experimental-feature Embedded %t/test.swift -c -o %t/a.o

// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.o | sort | tee %t/actual-dependencies.txt

// Fail if there is any entry in actual-dependencies.txt that's not in allowed-dependencies.txt
// With the Embedded Swift platform abstraction layer, the dependencies on the
// C library are replaced by the hooks in EmbeddedPlatform.h.
// RUN: %if !swift_embedded_platform && OS=linux-gnu %{ comm -13 %t/allowed-dependencies_linux.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: %if !swift_embedded_platform && !OS=linux-gnu %{ comm -13 %t/allowed-dependencies_macos.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: %if swift_embedded_platform && OS=linux-gnu %{ comm -13 %t/allowed-dependencies_linux_pal.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: %if swift_embedded_platform && !OS=linux-gnu %{ comm -13 %t/allowed-dependencies_macos_pal.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: test ! -s %t/extra.txt

//--- allowed-dependencies_macos.txt
___stack_chk_fail
___stack_chk_guard
___stdoutp
_flockfile
_free
_funlockfile
_memmove
_memset
_posix_memalign
_putchar

//--- allowed-dependencies_linux.txt
__stack_chk_fail
__stack_chk_guard
flockfile
free
funlockfile
memmove
memset
posix_memalign
putchar
stdout
//--- allowed-dependencies_macos_pal.txt
___stack_chk_fail
___stack_chk_guard
__swift_allocate
__swift_deallocate
__swift_lockStandardOutput
__swift_reportErrorAt
__swift_typedAllocate
__swift_typedDeallocate
__swift_unlockStandardOutput
__swift_writeToStandardOutput
_memmove
_memset
_putchar
//--- allowed-dependencies_linux_pal.txt
__stack_chk_fail
__stack_chk_guard
_swift_allocate
_swift_deallocate
_swift_lockStandardOutput
_swift_reportErrorAt
_swift_typedAllocate
_swift_typedDeallocate
_swift_unlockStandardOutput
_swift_writeToStandardOutput
memmove
memset
putchar
//--- test.swift
// RUN: %target-clang -x c -c %S/Inputs/print.c -o %t/print.o
// RUN: %target-embedded-link %t/a.o %t/print.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern
// REQUIRES: embedded_stdlib_default_codegen
// UNSUPPORTED: OS=linux-gnu && CPU=aarch64

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
  }
}
