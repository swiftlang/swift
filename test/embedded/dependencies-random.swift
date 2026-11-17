// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -disable-implicit-concurrency-module-import %t/test.swift -c -o %t/a.o

// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.o | sort | tee %t/actual-dependencies.txt

// Fail if there is any entry in actual-dependencies.txt that's not in allowed-dependencies.txt
// RUN: %if !swift_embedded_platform && OS=macosx %{ comm -13 %t/allowed-dependencies_macos.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: %if !swift_embedded_platform && OS=wasip1 %{ comm -13 %t/allowed-dependencies_wasi.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: %if !swift_embedded_platform && OS=macosx %{ test ! -s %t/extra.txt %}
// RUN: %if !swift_embedded_platform && OS=wasip1 %{ test ! -s %t/extra.txt %}

// Each list must stay sorted with no trailing blank line: GNU comm rejects
// unsorted input, while the BSD comm on Darwin silently accepts it.

// Without the Embedded Swift platform abstraction layer, Linux has two valid
// dependency sets, because the embedded runtime calls `arc4random_buf` when the
// C library provides it and `getrandom` when it doesn't (glibc older than
// 2.36).
// RUN: %if !swift_embedded_platform && OS=linux-gnu %{ comm -13 %t/allowed-dependencies_linux_arc4random.txt %t/actual-dependencies.txt > %t/extra_arc4random.txt %}
// RUN: %if !swift_embedded_platform && OS=linux-gnu %{ comm -13 %t/allowed-dependencies_linux_getrandom.txt %t/actual-dependencies.txt > %t/extra_getrandom.txt %}
// RUN: %if !swift_embedded_platform && OS=linux-gnu %{ test ! -s %t/extra_arc4random.txt || test ! -s %t/extra_getrandom.txt %}

// With the abstraction layer, the dependencies on the C library (including
// the source of randomness) are replaced by the hooks in EmbeddedPlatform.h,
// so there is a single dependency set per platform.
// RUN: %if swift_embedded_platform && OS=macosx %{ comm -13 %t/allowed-dependencies_macos_pal.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: %if swift_embedded_platform && OS=linux-gnu %{ comm -13 %t/allowed-dependencies_linux_pal.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: %if swift_embedded_platform && OS=wasip1 %{ comm -13 %t/allowed-dependencies_wasi_pal.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: %if swift_embedded_platform %{ test ! -s %t/extra.txt %}

//--- allowed-dependencies_macos.txt
___stack_chk_fail
___stack_chk_guard
___stdoutp
_arc4random_buf
_flockfile
_free
_funlockfile
_memmove
_memset
_posix_memalign
_putchar
//--- allowed-dependencies_linux_arc4random.txt
__stack_chk_fail
__stack_chk_guard
arc4random_buf
flockfile
free
funlockfile
memmove
memset
posix_memalign
putchar
stdout
//--- allowed-dependencies_linux_getrandom.txt
__errno_location
__stack_chk_fail
__stack_chk_guard
flockfile
free
funlockfile
getrandom
memmove
memset
posix_memalign
putchar
stdout
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
//--- allowed-dependencies_macos_pal.txt
___stack_chk_fail
___stack_chk_guard
__swift_allocate
__swift_deallocate
__swift_generateRandom
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
_swift_generateRandom
_swift_lockStandardOutput
_swift_reportErrorAt
_swift_typedAllocate
_swift_typedDeallocate
_swift_unlockStandardOutput
_swift_writeToStandardOutput
memmove
memset
putchar
//--- allowed-dependencies_wasi_pal.txt
__indirect_function_table
__memory_base
__stack_chk_fail
__stack_chk_guard
__stack_pointer
__table_base
_swift_allocate
_swift_deallocate
_swift_generateRandom
_swift_lockStandardOutput
_swift_reportErrorAt
_swift_typedAllocate
_swift_typedDeallocate
_swift_unlockStandardOutput
_swift_writeToStandardOutput
putchar
//--- test.swift
// RUN: %target-clang -x c -c %S/Inputs/print.c -o %t/print.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o %t/print.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// UNSUPPORTED: OS=linux-gnu && CPU=aarch64
// UNSUPPORTED: OS=emscripten
// arc4random_buf is not provided by emscripten's musl-derived libc
// (`wasm-ld: error: undefined symbol: arc4random_buf`).

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern
// REQUIRES: embedded_stdlib_default_codegen

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
