// RUN: %target-swift-frontend -parse -verify %s

enum MSV : ErrorType {
  case Foo, Bar, Baz

  var domain: String { return "" }
  var code: Int { return 0 }
}

func a() {}
func b() {}
func c() {}
func d() {}
func e() {}

func opaque_error() -> ErrorType { return MSV.Foo }

func one() {
  throw MSV.Foo // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
}

func two() {
  throw opaque_error() // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
}

func three() {
  do {
    throw opaque_error() // expected-error {{error is not handled because the enclosing catch is not exhaustive}}
  } catch let e as MSV {
    _ = e
  }
}

func four() {
  do {
    throw opaque_error()
  } catch let e {
    _ = e
  }
}

func five() {
  do {
    throw opaque_error()
  } catch let e as MSV {
    _ = e
  } catch _ {
  }
}

func six() {
  do {
    do {
      throw opaque_error()
    } catch let e as MSV {
      _ = e
    }
  } catch _ {
  }
}

func seven_helper() throws -> Int { throw MSV.Baz }

struct seven {
  var x: Int {
    do {
      return try seven_helper()
    } catch {
      return 0
    }
  }
}

class eight {
  lazy var bar: Int = {
    do {
      return try seven_helper()
    } catch {
      return 0
    }
  }()
}

protocol ThrowingProto {
  func foo() throws
  static func bar() throws
}

func testExistential(p : ThrowingProto) throws {
  try p.foo()
  try p.dynamicType.bar()
}
func testGeneric<P : ThrowingProto>(p : P) throws {
  try p.foo()
  try P.bar()
}