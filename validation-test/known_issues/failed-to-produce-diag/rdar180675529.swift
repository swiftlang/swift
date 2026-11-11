// rdar://180675529
// RUN: %target-typecheck-verify-swift -swift-version 5

enum Status {
  case unknown
  case enabled
  case disabled(Double?)
}

func getStatus() async throws -> Status { .unknown }

func setValue(_ value: Double?) {}

func test() {
  Task { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    if let (enabled, value) = try? await getStatus() {
      setValue(enabled ? 0.0 : value)
    }
  }
}
