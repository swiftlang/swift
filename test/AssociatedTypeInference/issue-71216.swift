// RUN: %target-typecheck-verify-swift

protocol RequiresIntFunction {
  associatedtype A = ()
  func intFunction(_ x: Int, _ a: A) -> Bool
}

struct Regular<A>: RequiresIntFunction {
  func intFunction(_ x: Int, _ a: A) -> Bool {
    true
  }
}

struct GenericBad<A>: RequiresIntFunction {
  func intFunction(_ x: some FixedWidthInteger, _ a: A) -> Bool {
    true
  }
}
