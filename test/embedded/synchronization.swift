// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature Embedded -disable-availability-checking -c -o %t/main.o
// RUN: %target-clang %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: synchronization
// REQUIRES: swift_feature_Embedded

import Synchronization

@main
struct Main {
  static let x = Atomic(128)

  static func main() {
    let old = x.load(ordering: .relaxed)
    x.store(42, ordering: .relaxed)
    let new = x.load(ordering: .relaxed)
    print(old) // CHECK: 128
    print(new) // CHECK: 42
    let old2 = x.exchange(12, ordering: .acquiring)
    print(old2) // CHECK: 42
    let (exchanged, original) = x.compareExchange(expected: 128, desired: 316, ordering: .sequentiallyConsistent)
    print(exchanged) // CHECK: false
    print(original) // CHECK: 12
    let (exchanged2, original2) = x.compareExchange(expected: 12, desired: 316, ordering: .sequentiallyConsistent)
    print(exchanged2) // CHECK: true
    print(original2) // CHECK: 12
  }
}
