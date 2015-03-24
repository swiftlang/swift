// RUN: %target-swift-frontend -emit-silgen -verify %s

enum MSV : _ErrorType {
  case Foo, Bar, Baz

  var domain: String { return "" }
  var code: Int { return 0 }
}

func a() {}
func b() {}
func c() {}
func d() {}
func e() {}

func opaque_error() -> _ErrorType { return MSV.Foo }

func one() {
  throw MSV.Foo // expected-error {{error is not handled and enclosing function is not marked with 'throws'}}
}

func two() {
  throw opaque_error() // expected-error {{error is not handled and enclosing function is not marked with 'throws'}}
}

func three() {
  do { // expected-error {{'catch' patterns must be exhaustive in function not marked with 'throws'}}
    throw opaque_error()
  } catch let e as MSV {
  }
}

func four() {
  do {
    throw opaque_error()
  } catch let e {
  }
}

func five() {
  do {
    throw opaque_error()
  } catch let e as MSV {
  } catch _ {
  }
}

func six() {
  do {
    do {
      throw opaque_error()
    } catch let e as MSV {
    }
  } catch _ {
  }
}
