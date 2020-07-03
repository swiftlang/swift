// RUN: %target-typecheck-verify-swift -parse-stdlib

// rdar://64890308: Make sure we don't leave one-way constraints unsolved.

import Swift

@_functionBuilder
class ArrayBuilder<Element> {
  static func buildBlock() -> [Element] { [] }
  static func buildBlock(_ elt: Element) -> [Element] { [elt] }
  static func buildBlock(_ elts: Element...) -> [Element] { elts }
}

func foo<T>(@ArrayBuilder<T> fn: () -> [T]) {}
foo {
  ""
}

struct S<T> {
  init(_: T.Type) {}
  func overloaded() -> [T] { [] }
  func overloaded(_ x: T) -> [T] { [x] }
  func overloaded(_ x: T...) -> [T] { x }
}

func bar<T>(_ x: T, _ fn: (T, T.Type) -> [T]) {}
bar("") { x, ty in
  (Builtin.one_way(S(ty).overloaded(x)))
}
