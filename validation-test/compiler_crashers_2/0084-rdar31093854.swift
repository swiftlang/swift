// RUN: not --crash %target-swift-frontend -swift-version 4 %s -typecheck -o /dev/null

// This should actually type check successfully.

protocol P {
  associatedtype T
}

protocol Q1 : P {
  typealias T = Int

  func f(_: T)
}

protocol Q2 : P {
  associatedtype T where T == Int

  func f(_: T)
}
