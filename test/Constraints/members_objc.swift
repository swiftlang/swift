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
  _ = T.bar // expected-error {{partial application of method in @objc protocol is not allowed}}
}

// rdar://problem/22012606 - test applications of subscript members of class-constrained protocols
@objc protocol subject_ClassConstrainedSubscript {
  subscript(index: Int) -> Int { get }
}

@objc class test_HasSubscript : subject_ClassConstrainedSubscript {
  subscript(index: Int) -> Int { get { return 0 } }
}

func test_subject_ClassConstrainedSubscript() {
  let list: subject_ClassConstrainedSubscript! = test_HasSubscript()
  list[0]
}