// RUN: %target-typecheck-verify-swift -enable-experimental-feature InitAccessors

func test_invalid_init_accessor_use() {
  var other: String = "" // expected-warning {{}}
  var x: Int {
    init(newValue) initializes(other) {}
    // expected-error@-1 {{init accessors could only be associated with properties}}

    get { 42 }
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
