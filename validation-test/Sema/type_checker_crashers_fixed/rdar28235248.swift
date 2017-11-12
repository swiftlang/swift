// RUN: not %target-swift-frontend %s -typecheck

protocol II {
  associatedtype E
}

protocol P {
  associatedtype I : II
  associatedtype X
}

extension P where I.E : P, I.E.X.D == Int, I.E.X == Int {}
