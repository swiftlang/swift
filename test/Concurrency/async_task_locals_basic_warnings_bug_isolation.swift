// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -target %target-swift-5.1-abi-triple

// RUN: %target-swift-frontend -I %t -plugin-path %swift-plugin-dir -target %target-swift-5.1-abi-triple -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency
// REQUIRES: asserts

actor Test {

  @TaskLocal static var local: Int?

  func withTaskLocal(isolation: isolated (any Actor)? = #isolation,
                     _ body: (consuming NonSendableValue, isolated (any Actor)?) -> Void) async {
    Self.$local.withValue(12) {
      body(NonSendableValue(), isolation)
    }
  }
}

class NonSendableValue {}
