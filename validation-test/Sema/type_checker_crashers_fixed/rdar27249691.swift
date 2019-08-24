// RUN: not %target-swift-frontend %s -typecheck

infix operator ~> : BitwiseShiftPrecedence

struct F {}

protocol P {
  associatedtype R
  func X() -> R
  static func ~> (: Self, _: F) -> R
}

func ~> <T:P> (: T, _: F) -> T.R {
  return X()
}
