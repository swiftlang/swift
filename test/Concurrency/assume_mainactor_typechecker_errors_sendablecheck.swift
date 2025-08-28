// RUN: %target-swift-frontend -swift-version 5 -emit-sil -default-isolation MainActor %s -verify -verify-additional-prefix swift5-
// RUN: %target-swift-frontend -swift-version 6 -emit-sil -default-isolation MainActor %s -verify -verify-additional-prefix swift6-

// Ensure that a Sendable-conforming protocol suppresses @MainActor inference
// for a type.
enum CK: CodingKey {
  case one

  func f() { }

  struct Nested {
    func g() { }
  }
}

nonisolated func testCK(x: CK, y: CK.Nested) {
  x.f() // okay, because CK and CK.f are not @MainActor.
  y.g()
}
