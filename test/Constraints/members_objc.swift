// RUN: %target-typecheck-verify-swift -enable-objc-interop

import Swift

@objc
protocol P2 {
  func bar(_ x: Int)
  static func pub(_ x: Int)
}

func existential(_ p2 : P2) {
  _ = p2.bar
  _ = P2.bar
}

func archetype<T: P2>(_ p2 : T) {
  _ = p2.bar
  _ = T.bar
  _ = T.pub
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
  _ = list[0]
}
