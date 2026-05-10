// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
  associatedtype B
}

protocol Q: P {}

extension Q where A == Int {
  typealias B = Int
}
