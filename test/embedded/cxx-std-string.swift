// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-experimental-feature Embedded -cxx-interoperability-mode=default -wmo -parse-as-library -c -o %t/main.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o -lc++ %target-embedded-posix-shim -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

import CxxStdlib

@main
struct Main {
  static func main() {
    var s = std.string("hello")
    s += std.string(", world")

    // std.string -> Swift.String, via the overlay's initializer.
    print(String(s))
    // CHECK: hello, world

    // Swift.String -> std.string, and the Equatable conformance.
    print(std.string("hello, world") == s)
    // CHECK-NEXT: true

    print(s.empty())
    // CHECK-NEXT: false

    // Collection conformance from the overlay.
    print(s.count)
    // CHECK-NEXT: 12
  }
}
