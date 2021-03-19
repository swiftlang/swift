// RUN: %target-typecheck-verify-swift -enable-infer-public-sendable

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
