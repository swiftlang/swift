// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target arm64-apple-none-macho -Xcc -D__MACH__ -Xcc -D__arm64__ -Xcc -D__APPLE__ \
// RUN:   -enable-experimental-feature Embedded -enforce-exclusivity=none %s -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: VENDOR=apple
// REQUIRES: CPU=arm64

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

print("Hello, Embedded Swift!")
// CHECK: Hello, Embedded Swift!
