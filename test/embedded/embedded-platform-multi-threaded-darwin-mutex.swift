// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -disable-availability-checking -wmo %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-multi-threaded-darwin-shim -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: synchronization
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_embedded_platform

import Synchronization

@main
struct Main {
  static func main() {
    let mutex = Mutex(0)

    let first = mutex.withLock { value -> Int in
      value += 1
      return value
    }
    print(first)
    // CHECK: 1

    let second = mutex.withLockIfAvailable { value -> Int in
      value += 41
      return value
    }
    print(second ?? -1)
    // CHECK: 42

    print("done")
    // CHECK: done
  }
}
