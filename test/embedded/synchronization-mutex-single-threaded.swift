// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -disable-availability-checking -wmo %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-single-threaded-shim %target-embedded-synchronization -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: synchronization
// REQUIRES: swift_feature_Embedded

import Synchronization

var result = 0

@main
struct Main {
  static func main() {
    let mutex = Mutex(0)

    mutex.withLock {
      $0 += 1
    }

    let value = mutex.withLockIfAvailable {
      $0 += 1
      return $0
    }

    result = value!
  }
}
