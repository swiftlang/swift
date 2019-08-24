// RUN: %target-swift-frontend -module-name main -typecheck -verify -swift-version 4 %s

// Some convenient test points to escape closures to
var x: (Int) -> Int = { $0 }
var y: (inout Int, Int) -> Void = { $0 = $1 }
var z: (Int, Int, Int) throws -> Int = { $2 }

func escapeX(_ xx: (Int) -> Int, _ value: Int) { // expected-note* {{non-escaping}}
  x = xx // expected-error{{non-escaping parameter}}
  withoutActuallyEscaping(xx) { escapableXX in
    x = xx // expected-error{{non-escaping parameter}}
    x = escapableXX
    x = xx // expected-error{{non-escaping parameter}}

    _ = x(value)
    _ = xx(value)
    _ = escapableXX(value)
  }
  withoutActuallyEscaping(xx, do: { escapableXX in
    x = escapableXX
  })
  Swift.withoutActuallyEscaping(xx) { escapableXX in
    x = escapableXX
  }
  Swift.withoutActuallyEscaping(xx, do: { escapableXX in
    x = escapableXX
  })
  x = xx // expected-error{{non-escaping parameter}}
}

func escapeY(_ yy: (inout Int, Int) -> Void, _ value: inout Int) { // expected-note{{non-escaping}}
  y = yy // expected-error{{non-escaping parameter}}
  withoutActuallyEscaping(yy) { escapableYY in
    y = escapableYY

    y(&value, value)
    yy(&value, value)
    escapableYY(&value, value)
  }
}

func escapeZ(_ zz: (Int, Int, Int) throws -> Int) { // expected-note{{non-escaping}}
  z = zz // expected-error{{non-escaping parameter}}
  withoutActuallyEscaping(zz) { escapableZZ in
    z = escapableZZ
  }
}

func returnThroughWAE(_ xx: (Int) -> Int, _ value: Int) -> Int {
  return withoutActuallyEscaping(xx) { escapableXX in
    x = escapableXX
    return x(value)
  }
}

func rethrowThroughWAE(_ zz: (Int, Int, Int) throws -> Int, _ value: Int) throws {
  try withoutActuallyEscaping(zz) { escapableZZ in
    _ = try zz(value, value, value)
    _ = try escapableZZ(value, value, value)
  }
}

let _: ((Int) -> Int, (@escaping (Int) -> Int) -> ()) -> ()
  = withoutActuallyEscaping(_:do:) // expected-error{{}}


// Failing to propagate @noescape into non-single-expression
// closure passed to withoutActuallyEscaping

// https://bugs.swift.org/browse/SR-7886

class Box<T> {
  let value: T

  init(_ value: T) {
    self.value = value
  }

  func map1<U>(_ transform: (T) -> U) -> Box<U> {
    return withoutActuallyEscaping(transform) { transform in
      return Box<U>(transform(value))
    }
  }

  func map2<U>(_ transform: (T) -> U) -> Box<U> {
    return withoutActuallyEscaping(transform) { transform in
      let v = Box<U>(transform(value))
      return v
    }
  }
}
