// RUN: %target-parse-verify-swift

struct Throws1 {
  init() throws { }
}




// <rdar://problem/22108568> Infinite recursion in typeCheckChildIndependently()
func unwrap() -> Int {}

class A22108568 {
  init(a: ()) {}
}

class B22108568 : A22108568 {
  required init() {
    try super.init(a: unwrap()) // expected-error {{cannot convert call result type 'Int' to expected type '()'}}
  }
}

