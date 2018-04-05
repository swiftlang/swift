// RUN: %target-typecheck-verify-swift %s

class A {
  func foo() {
    class B {
      let question: String = "ultimate question"
      let answer: Int? = 42

      lazy var bar: () -> String = { [weak self] in
        guard let self = self else {
          return "Unknown"
        }

        if let answer = self.answer {
          return "\(self.question) = \(answer)"
        } else {
          return "<\(self.question) />"
        }
      }
    }
  }
}
