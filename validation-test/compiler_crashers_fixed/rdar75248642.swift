// RUN: not %target-swift-frontend -typecheck %s

precedencegroup MyOperatorPrecedence {
  lowerThan: MultiplicationPrecedence
  higherThan: AdditionPrec
  associativity: left
  assignment: false
}
