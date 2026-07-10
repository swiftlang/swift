// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -target %target-cpu-apple-ios99-macabi -parse-as-library -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-embedded-link -target %target-cpu-apple-ios99-macabi %t/main.o %target-embedded-posix-shim -o %t/a.out -dead_strip
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx
// REQUIRES: maccatalyst_support
// REQUIRES: swift_feature_Embedded

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
