// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded %s -c -o %t/a.o

// backslash is not doing anything, just to make the grep not match its own line
// RUN: grep DEP-%target-os %s | sed 's#// DEP-%target-os: ##' | sort > %t/expected-dependencies.txt
// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.out | sort > %t/actual-dependencies.txt
// RUN: diff -u %t/expected-dependencies.txt %t/actual-dependencies.txt

// DEP-macosx: ___stack_chk_fail
// DEP-macosx: ___stack_chk_guard
// DEP-macosx: _free
// DEP-macosx: _posix_memalign
// DEP-macosx: _printf
// DEP-macosx: _putchar
// DEP-macosx: dyld_stub_binder

// DEP-linux-gnu: __stack_chk_fail
// DEP-linux-gnu: __stack_chk_guard
// DEP-linux-gnu: free
// DEP-linux-gnu: posix_memalign
// DEP-linux-gnu: putchar

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu

@_silgen_name("putchar")
func putchar(_: UInt8)

public func print(_ s: StaticString, terminator: StaticString = "\n") {
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

print("Hello Embedded Swift!") // CHECK: Hello Embedded Swift!
