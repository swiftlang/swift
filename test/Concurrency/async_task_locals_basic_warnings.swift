// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -target %target-swift-5.1-abi-triple

// RUN: %target-swift-frontend -I %t -plugin-path %swift-plugin-dir -target %target-swift-5.1-abi-triple -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -I %t -plugin-path %swift-plugin-dir -disable-availability-checking -swift-version 6 -parse-as-library %s -emit-sil -o /dev/null -verify

// In MainActor isolation by default mode, the synthesized $variable
// must not become MainActor isolated as that would be nonsensical
// RUN: %target-swift-frontend -I %t -plugin-path %swift-plugin-dir -disable-availability-checking -swift-version 6 -default-isolation MainActor -parse-as-library %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency

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

@globalActor
struct OtherGlobalActor {
  static let shared = Test()
}

class TaskLocalContainer {
  @TaskLocal nonisolated static var number: Int = 42

  // It is allowed to @GlobalActor the variable although not very useful... 
  // Accesses may be performed from child tasks anyway,
  // so this doesn't guarantee only accesses from the specified actor.
  @TaskLocal @OtherGlobalActor static var other: Int = 7
}
