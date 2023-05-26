// RUN: %target-typecheck-verify-swift -enable-experimental-feature InitAccessors

func test_invalid_init_accessor_use() {
  var other: String = "" // expected-warning {{}}
  var x: Int {
    init(newValue) initializes(other) {}
    // expected-error@-1 {{init accessors could only be associated with properties}}

    get { 42 }
  }

  struct X {
    subscript(x: Int) -> Bool {
      init(newValue) {} // expected-error {{init accessors could only be associated with properties}}

      get { false }
    }
  }
}

func test_use_of_initializes_accesses_on_non_inits() {
  struct Test1 {
    var x: Int
    var y: String

    var _x: Int {
      init(newValue) initializes(x) accesses(y) { // Ok
      }

      get { x }
    }

    var _y: String {
      get { y }
      set(newValue) initializes(y) {}
      // expected-error@-1 {{initalizes(...) attribute could only be used with init accessors}}
    }

    var _q: String {
      get { y }
      set(newValue) accesses(x) {}
      // expected-error@-1 {{accesses(...) attribute could only be used with init accessors}}
    }
  }
}

func test_invalid_refs_in_init_attrs() {
  struct Test {
    var c: Int { get { 42 } }
    var x: Int {
      init(newValue) initializes(a) accesses(b c) {}
      // expected-error@-1 {{find type 'a' in scope}}
      // expected-error@-2 {{find type 'b' in scope}}
      // expected-error@-3 {{init accessor cannot refer to property 'c'; init accessors can refer only to stored properties}}
    }

    var y: String {
      init(newValue) initializes(test) {}
      // expected-error@-1 {{ambiguous reference to member 'test'}}
    }

    func test(_: Int) {} // expected-note {{'test' declared here}}
    func test(_: String) -> Int { 42 } // expected-note {{'test' declared here}}
  }
}
