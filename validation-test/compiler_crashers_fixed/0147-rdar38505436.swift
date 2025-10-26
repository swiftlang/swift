// RUN: not %target-typecheck-verify-swift

protocol P1 {
  class N1 {
    init() {}
  }
}

protocol P2 {}

extension P2 {
  class N2 {
    init() {}
  }
}

class C1: P1.N1 {
  override init() {}
}

class C2: P2.N2 {
  override init() {}
}
