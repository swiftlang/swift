// RUN: not %target-swift-frontend %s -typecheck

protocol A {
  var question: String { get }

  struct B {
    var answer: Int = 42

    func foo(a: A) {
      _ = a.question
    }
  }
}

class C : A {
  var question: String = "ultimate question"

  func foo() -> B {}
  func bar() -> A.B {}
  func baz(b: B) {
    _ = b.answer
  }
}

class D : A {
  var question: String = ""

  struct E {
    func baz(b: B) {
      _ = b.answer
    }
  }
}

class F<T> : A {
  var question: String = ""

  func foo(b: B) {
    _ = b.answer
  }
}
