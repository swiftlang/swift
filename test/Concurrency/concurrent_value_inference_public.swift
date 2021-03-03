// RUN: %target-typecheck-verify-swift -enable-infer-public-concurrent-value

func acceptCV<T: ConcurrentValue>(_: T) { }

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
