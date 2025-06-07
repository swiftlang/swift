// RUN: %target-swift-emit-sil -verify %s -o /dev/null

func foo() -> Int {
  switch Bool.random() {
  case true:
    0 // expected-warning {{integer literal is unused}}
  case false:
    do {}
  }
} // expected-error {{missing return in global function expected to return 'Int'}}

enum E<T> {
  case x(T), y

  func foo() -> E<T> {
    switch self {
    case .x:
      while true {}
    case .y:
      fatalError()
    }
  }
}
