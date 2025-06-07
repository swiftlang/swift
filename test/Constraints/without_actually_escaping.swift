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
    x = xx

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

let _: ((Int) -> Int, (@escaping (Int) -> Int) -> ()) -> () = withoutActuallyEscaping(_:do:)
// expected-error@-1 {{invalid conversion from 'async' function of type '((Int) -> Int, (@escaping (Int) -> Int) async -> ()) async -> ()' to synchronous function type '((Int) -> Int, (@escaping (Int) -> Int) -> ()) -> ()'}}


// Failing to propagate @noescape into non-single-expression
// closure passed to withoutActuallyEscaping

// https://github.com/apple/swift/issues/50421

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

enum HomeworkError: Error {
  case forgot
  case dogAteIt
}

enum MyError: Error {
  case fail
}

func letEscapeThrowTyped(f: () throws(HomeworkError) -> () -> ()) throws(HomeworkError) -> () -> () {
  // Note: thrown error type inference for closures will fix this error below.
  return try withoutActuallyEscaping(f) { return try $0() }
  // expected-error@-1{{thrown expression type 'any Error' cannot be converted to error type 'HomeworkError'}}
}

func letEscapeThrowTypedBad(f: () throws(HomeworkError) -> () -> ()) throws(MyError) -> () -> () {
  // Note: thrown error type inference for closures will change this error below.
  return try withoutActuallyEscaping(f) { return try $0() }
  // expected-error@-1{{thrown expression type 'any Error' cannot be converted to error type 'MyError'}}
}
