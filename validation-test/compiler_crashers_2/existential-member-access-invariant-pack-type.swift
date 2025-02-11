// RUN: not --crash %target-swift-frontend -typecheck -target %target-swift-5.9-abi-triple %s

struct Variadic<each A> {}

protocol VariadicResult {
  associatedtype A
  func foo() -> Variadic<A>
}

func variadicResult(a: any VariadicResult) {
  a.foo()
}
