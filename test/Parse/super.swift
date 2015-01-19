// RUN: %target-parse-verify-swift

class B {
  var foo: Int
  func bar() {}

  init() {}
  init(x: Int) {}

  subscript(x: Int) -> Int {
    get {}
    set {}
  }
}

class D : B {
  override init() {
    super.init()
  }

  override init(x:Int) {
    super.init //  expected-error {{'super.init' cannot be referenced without arguments}} expected-error {{could not find an overload for 'init' that accepts the supplied arguments}}
  }

  func super_calls() {
    super.foo        // expected-error {{expression resolves to an unused l-value}}
    super.foo.bar    // expected-error {{'Int' does not have a member named 'bar'}}
    super.bar        // expected-error {{expression resolves to an unused function}}
    super.bar()
    super.init // expected-error{{'super.init' cannot be called outside of an initializer}}
    super.init() // expected-error{{'super.init' cannot be called outside of an initializer}}
    super.init(0) // expected-error{{'super.init' cannot be called outside of an initializer}}
    super[0]        // expected-error {{expression resolves to an unused l-value}}
  }

  func bad_super_1() {
    super.$0 // expected-error{{expected identifier or 'init'}}
  }

  func bad_super_2() {
    super(0) // expected-error{{expected '.' or '[' after 'super'}}
  }
}
