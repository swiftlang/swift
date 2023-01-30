// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -disable-availability-checking
// RUN: %target-typecheck-verify-swift -I %t  -disable-availability-checking -warn-concurrency -parse-as-library
// REQUIRES: concurrency

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