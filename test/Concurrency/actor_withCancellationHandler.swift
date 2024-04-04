// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -disable-availability-checking

// RUN: %target-swift-frontend -I %t  -disable-availability-checking -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -I %t  -disable-availability-checking -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts

actor foo {
  var t: Task<Void, Error>?

  func access() {}

  func bar() {
    self.t = Task {
      await withTaskCancellationHandler {
        self.access()
      } onCancel: { @Sendable in

      }
    }
  }
}
