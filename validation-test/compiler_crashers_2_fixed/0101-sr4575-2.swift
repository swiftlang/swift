// RUN: not %target-swift-frontend %s -typecheck

struct V<T> {}
extension V : BidirectionalCollection {}
struct S {
  func bar<T>(_ to: T.Type) -> V<T> {
    return V<T>()
  }

  func foo() {
    let _ = Array(self.bar(UTF16.self)) // expected-error {{generic parameter 'Element' could not be inferred}}
  }
}
