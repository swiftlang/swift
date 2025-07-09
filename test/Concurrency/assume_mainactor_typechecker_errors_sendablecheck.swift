// RUN: %target-swift-frontend -swift-version 5 -emit-sil -default-isolation MainActor %s -verify -verify-additional-prefix swift5-
// RUN: %target-swift-frontend -swift-version 6 -emit-sil -default-isolation MainActor %s -verify -verify-additional-prefix swift6-

// Ensure that a Sendable-conforming protocol suppresses @MainActor inference
// for a type.
enum CK: CodingKey {
  case one

  func f() { }
}

nonisolated func testCK(x: CK) {
  x.f() // okay, because CK and CK.f are not @MainActor.
}
