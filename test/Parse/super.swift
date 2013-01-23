// RUN: %swift %s -dump-parse -verify

class B {
  var foo : (bar:Int)
  func bar() {}

  constructor() {}
  constructor(x:Int) {}

  subscript(x:Int) -> Int {get:}
}

class D {
  func super_calls() {
    super.foo
    super.foo.bar
    super.bar
    super.bar()
    super.constructor
    super.constructor()
    super.constructor(0)
    super[0]
  }

  func bad_super_1() {
    super.$0 // expected-error{{expected identifier or 'constructor'}}
  }

  func bad_super_2() {
    super(0) // expected-error{{expected '.' or '[' after 'super'}}
  }
}
