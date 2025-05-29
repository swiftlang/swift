// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -target %target-swift-5.1-abi-triple

// RUN: %target-swift-frontend -I %t -plugin-path %swift-plugin-dir -target %target-swift-5.1-abi-triple -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -I %t -plugin-path %swift-plugin-dir -target %target-swift-5.1-abi-triple -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify -enable-upcoming-feature RegionBasedIsolation
// RUN: %target-swift-frontend -I %t -plugin-path %swift-plugin-dir -disable-availability-checking -swift-version 6 -parse-as-library %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

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
