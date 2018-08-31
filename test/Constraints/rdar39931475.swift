// RUN: %target-typecheck-verify-swift

protocol P {
  func b(i: @escaping (inout Int) -> Double)
}

func foo<T: P>(_ bar: T) {
  _ = bar.b { a in Double((a, a += 1).0) }
}
