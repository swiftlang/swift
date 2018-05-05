// RUN: %target-swift-emit-silgen -enable-sil-ownership -verify %s

protocol AP {
  associatedtype B: BP
  var b: B { get }
}
protocol BP {}

func foo<A: AP>(x: A) -> A {
  func bar<B: BP>(x: B) {
  }
  func bas<B: BP>(x: B) {
    bar(x: x)
  }

  func bang() -> A { return x }
  func bong(_: A) {}

  let x = bang()
  bong(x)
}
