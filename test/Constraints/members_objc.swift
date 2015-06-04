// RUN: %target-parse-verify-swift

import Swift

@objc
protocol P2 {
  func bar(x: Int)
}

func existential(p2 : P2) {
  _ = p2.bar // expected-error{{partial application of method in @objc protocol is not allowed}}
}

func archetype<T: P2>(p2 : T) {
  _ = p2.bar // expected-error{{partial application of method in @objc protocol is not allowed}}
}

func archetypeMeta<T: P2>(p2 : T) {
  _ = T.bar // FIXME: not yet diagnosed
}
