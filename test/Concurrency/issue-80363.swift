// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/80363

class C {}

func testLocal() {
  isolated let c = C()
  // expected-error@-1 {{'isolated' may only be used on 'deinit' declarations}}
  _ = c

  isolated func test() {
    // expected-error@-1 {{'isolated' may only be used on 'deinit' declarations}}
  }
}

isolated var x: Int = 42
// expected-error@-1 {{'isolated' may only be used on 'deinit' declarations}}

isolated class Test {
  // expected-error@-1 {{'isolated' may only be used on 'deinit' declarations}}
}

struct TestMembers {
  isolated var q: String {
    // expected-error@-1 {{'isolated' may only be used on 'deinit' declarations}}
    get {
      "ultimate question"
    }

    isolated set {
      // expected-error@-1 {{expected 'get', 'set', 'willSet', or 'didSet' keyword to start an accessor definition}}
    }
  }

  isolated let a: Int = 42
  // expected-error@-1 {{'isolated' may only be used on 'deinit' declarations}}

  isolated subscript(x: Int) -> Bool {
    // expected-error@-1 {{'isolated' may only be used on 'deinit' declarations}}
    get { true }
  }

  isolated func test() {
    // expected-error@-1 {{'isolated' may only be used on 'deinit' declarations}}
  }
}
