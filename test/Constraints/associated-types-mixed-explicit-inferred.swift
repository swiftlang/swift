// RUN: %target-swift-frontend -typecheck -verify %s
protocol Foo {
  associatedtype Flim
  associatedtype Flam
  func foo(_: Flim) -> Flam
}

struct Bar: Foo {
  typealias Flim = Int

  func foo(_: Int) -> Int {}
  func foo(_: String) -> String {}
}

func testDeducedFlamType<T: Foo, U>(_: T, _: U.Type)
where T.Flam == U {}

testDeducedFlamType(Bar(), Int.self)

struct Bas<T, U, V, W>: Foo {
  typealias Flim = T
  func foo(_: T) -> U {}
  func foo(_: V) -> W {}
}

testDeducedFlamType(Bas<Int, String, Float, Double>(), String.self)
testDeducedFlamType(Bas<String, Float, Double, Int>(), Float.self)
