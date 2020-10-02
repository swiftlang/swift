// RUN: %target-typecheck-verify-swift

// Ambiguity when calling.
func ambiguous1( // expected-note 3 {{declared here}}

  x: (Int) -> Int = { $0 },
  a: Int = 5,
  y: (Int) -> Int = { $0 },
  b: Int = 5,
  z: (Int) -> Int = { $0 },
  c: Int = 5
) {}

func testAmbiguous1() {
  ambiguous1 { $0 } // expected-warning{{backward matching of the unlabeled trailing closure is deprecated; label the argument with 'z' to suppress this warning}}{{13-13=(z: }}{{20-20=)}}

  ambiguous1() { $0 } // expected-warning{{backward matching of the unlabeled trailing closure is deprecated; label the argument with 'z' to suppress this warning}}{{14-15=z: }}{{22-22=)}}

  ambiguous1(a: 3) { $0 } // expected-warning{{backward matching of the unlabeled trailing closure is deprecated; label the argument with 'z' to suppress this warning}}{{18-19=, z: }}{{26-26=)}}

  // No warning; this is matching the last parameter.
  ambiguous1(b: 3) { $0 }
}

// Ambiguity with two unlabeled arguments.
func ambiguous2( // expected-note{{declared here}}
  _: (Int) -> Int = { $0 },
  a: Int = 5,
  y: (Int) -> Int = { $0 },
  b: Int = 5,
  _: (Int) -> Int = { $0 },
  x: Int = 5
) {}

func testAmbiguous2() {
    ambiguous2 { $0 } // expected-warning{{backward matching of the unlabeled trailing closure is deprecated; label the argument with '_' to suppress this warning}}{{15-15=(}}{{22-22=)}}
}

// Ambiguity with one unlabeled argument.
func ambiguous3( // expected-note{{declared here}}
  x: (Int) -> Int = { $0 },
  a: Int = 5,
  y: (Int) -> Int = { $0 },
  b: Int = 5,
  _: (Int) -> Int = { $0 },
  c: Int = 5
) {}

func testAmbiguous3() {
  ambiguous3 { $0 } // expected-warning{{backward matching of the unlabeled trailing closure is deprecated; label the argument with '_' to suppress this warning}}{{13-13=(}}{{20-20=)}}
}

// Not ambiguous because of an arity mismatch that would lead to different
// type-checks.
func notAmbiguous1(
  x: (Int, Int) -> Int = { $0 + $1 },
  a: Int = 5,
  y: (Int) -> Int = { $0 },
  b: Int = 5,
  _: (Int) -> Int = { $0 },
  c: Int = 5
) { }

func testNotAmbiguous1() {
  notAmbiguous1 { $0 / $1 }
}

// Not ambiguous because of a missing default argument.
func notAmbiguous2(
  x: (Int) -> Int,
  a: Int = 5,
  y: (Int) -> Int = { $0 },
  b: Int = 5,
  _: (Int) -> Int = { $0 },
  c: Int = 5
) { }

func testNotAmbiguous2() {
  notAmbiguous2 { $0 }
}

// Not ambiguous because of a missing default argument.
func notAmbiguous3(
  x: (Int) -> Int = { $0 },
  a: Int = 5,
  y: (Int) -> Int = { $0 },
  b: Int = 5,
  _: (Int) -> Int ,
  c: Int = 5
) { }

func testNotAmbiguous3() {
  notAmbiguous3 { $0 }
}

// Ambiguous subscript
struct S {
  subscript( // expected-note {{'subscript(a:_:_:)' declared here}}
    a a: Int,
    fn1: (() -> Void)? = nil,
    fn2: (() -> Void)? = nil) -> Bool {
    get { return true }
  }

  subscript( // expected-note {{'subscript(b:_:fn2:)' declared here}}
    b b: Int,
    fn1: (() -> Void)? = nil,
    fn2 fn2: (() -> Void)? = nil) -> Bool {
    get { return true }
  }

  static func foo( // expected-note {{'foo(c:fn1:fn2:)' declared here}}
    c: Int,
    fn1: (() -> Void)? = nil,
    fn2: (() -> Void)? = nil) -> S {
    return S()
  }
}

func test_ambiguous_subscript_unresolved_member(s: S) {
  _ = s[a: 42] {} // expected-warning {{backward matching of the unlabeled trailing closure is deprecated; label the argument with '_' to suppress this warning}} {{14-15=, }} {{18-18=]}}
  _ = s[b: 42] {} // expected-warning {{backward matching of the unlabeled trailing closure is deprecated; label the argument with 'fn2' to suppress this warning}} {{14-15=, fn2: }} {{18-18=]}}
  let _: S = .foo(c: 42) {} // expected-warning {{backward matching of the unlabeled trailing closure is deprecated; label the argument with 'fn2' to suppress this warning}} {{24-25=, fn2: }} {{28-28=)}}
}
