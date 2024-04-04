// RUN: %target-swift-frontend -enable-infer-public-sendable %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -enable-infer-public-sendable %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -enable-infer-public-sendable %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend -enable-infer-public-sendable %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: asserts

func acceptCV<T: Sendable>(_: T) { }

public struct PublicStruct {
  var i: Int
}

public enum PublicEnum {
  case some
}

func testCV(ps: PublicStruct, pe: PublicEnum) {
  acceptCV(ps)
  acceptCV(pe)
}
