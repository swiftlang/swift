// RUN: %target-typecheck-verify-swift

/////////////

struct G<T> {
  var t: T
}

func foo1(x: (x: Int, y: Int)?, y: (Int, Int)) -> G<(x: Int, y: Int)> {
  let g = G(t: x ?? y)
  return g
}

func foo2(x: (Int, Int)?, y: (x: Int, y: Int)) -> G<(Int, Int)> {
  let g = G(t: x ?? y)
  return g
}

/////////////

func id<T>(_: T) -> T {}

func bar1(x: (x: Int, y: Int)) {
  func f(_: (Int, Int)) {}
  f(id(x))
}

func bar2(x: (Int, Int)) {
  func f(_: (x: Int, y: Int)) {}
  f(id(x))
}

/////////////

func unwrap<T>(_: T?) -> T {}

func baz1(x: (x: Int, y: Int)?) {
  func f(_: (Int, Int)) {}
  f(unwrap(x))
}

func baz2(x: (Int, Int)?) {
  func f(_: (x: Int, y: Int)) {}
  f(unwrap(x))
}
