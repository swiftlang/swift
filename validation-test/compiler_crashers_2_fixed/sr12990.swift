// RUN: %target-swift-frontend -typecheck %s -verify

class ContainerTransition {
  var viewControllers: [Int: String]?
  func completeTransition() {
    viewControllers?[Int//.max
    // expected-error@-1 {{no exact matches in call to subscript}}
    // expected-note@-2 {{found candidate with type '((Int).Type) -> Dictionary<Int, String>.SubSequence' (aka '(Int.Type) -> Slice<Dictionary<Int, String>>')}}
    // expected-note@-3 {{to match this opening '['}}
  } // expected-error {{expected ']' in expression list}}
}
