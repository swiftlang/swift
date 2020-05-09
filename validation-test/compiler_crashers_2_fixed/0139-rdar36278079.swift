// RUN: not %target-swift-frontend -typecheck %s

struct S {
  // presence of a static instance seems to be
  // necessary to cause this problem
  static let s = S()
}

protocol P {
  associatedtype T
  init(t: T)
}

extension S: P {
// Uncomment to stop assertion:
//  init(t: Int) {
//    self = S()
//  }
}
