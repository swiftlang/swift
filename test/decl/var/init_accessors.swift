// RUN: %target-typecheck-verify-swift -enable-experimental-feature InitAccessors

func test_invalid_init_accessor_use() {
  var other: String = "" // expected-warning {{}}
  var x: Int {
    init(newValue) initializes(other) {}
    // expected-error@-1 {{init accessors could only be associated with properties}}

    get { 42 }
  }
}
