// RUN: %target-swift-frontend %s -verify -emit-sil -o /dev/null

struct R<T> {
  var str: String?
}

func map<A, B>(e: (A) -> B) -> () -> R<B> {
  fatalError()
}
func map<A, B>(_ : (A) -> B) -> (A?) -> B? {
  fatalError()
}

infix operator |>
func |> <A, B> (g: A, h: (A) -> B) -> B { h(g) }

infix operator ^^^
func ^^^ <A, B, C>(j: ((B) -> C) -> A, k: String) {}

extension WritableKeyPath {
  static func ^^^ (l: WritableKeyPath, m: Value) -> (Root) -> Root {
    fatalError()
  }
}

func foo<T>(_ s: String, _ rt: R<T>?) -> String? {
  return rt.flatMap { _ in
    rt |> map(\.str ^^^ s)
  }
  .flatMap(\.str)
}
