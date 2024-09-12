// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature Embedded -throws-as-traps -enable-builtin-module -c -o %t/main.o
// RUN: %target-clang %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

// The Darwin SDK overlay module in the macOS SDK cannot be imported in
// embedded Swift mode.
// XFAIL: OS=macosx

import Darwin

@main
struct Main {
  static func main() {
    let x = getuid()
    print("User id: ")
    print(x)
  }
}

// CHECK: User id:
