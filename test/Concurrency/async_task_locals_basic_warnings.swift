// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -disable-availability-checking

// RUN: %target-swift-frontend -I %t  -disable-availability-checking -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -I %t  -disable-availability-checking -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts

actor Test {

  @TaskLocal static var local: Int?

  func run() async {
    // This should NOT produce any warnings, the closure withValue uses is @Sendable:
    await Self.$local.withValue(42) {
      await work()
    }
  }

  func work() async {
    print("Hello \(Self.local ?? 0)")
  }
}
