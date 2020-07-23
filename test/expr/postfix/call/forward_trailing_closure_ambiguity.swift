// RUN: %target-typecheck-verify-swift

// Ambiguity when calling.
func ambiguous1( // expected-note 3 {{'ambiguous1(x:a:y:b:z:c:)' contains defaulted closure parameters '}}

  x: (Int) -> Int = { $0 },
  a: Int = 5,
  y: (Int) -> Int = { $0 },
  b: Int = 5,
  z: (Int) -> Int = { $0 },
  c: Int = 5
) {}

func testAmbiguous1() {
  ambiguous1 { $0 } // expected-warning{{since Swift 5.3, unlabeled trailing closure argument matches parameter 'x' rather than parameter 'z'}}
    // expected-note@-1{{label the argument with 'z' to retain the pre-Swift 5.3 behavior}}{{13-13=(z: }}{{20-20=)}}
    // expected-note@-2{{label the argument with 'x' to silence this warning for Swift 5.3 and newer}}{{13-13=(x: }}{{20-20=)}}

  ambiguous1() { $0 } // expected-warning{{since Swift 5.3, unlabeled trailing closure argument matches parameter 'x' rather than parameter 'z'}}
    // expected-note@-1{{label the argument with 'z' to retain the pre-Swift 5.3 behavior}}{{14-15=z: }}{{22-22=)}}
    // expected-note@-2{{label the argument with 'x' to silence this warning for Swift 5.3 and newer}}{{14-15=x: }}{{22-22=)}}

  ambiguous1(a: 3) { $0 } // expected-warning{{since Swift 5.3, unlabeled trailing closure argument matches parameter 'y' rather than parameter 'z'}}
    // expected-note@-1{{label the argument with 'z' to retain the pre-Swift 5.3 behavior}}{{18-19=, z: }}{{26-26=)}}
    // expected-note@-2{{label the argument with 'y' to silence this warning for Swift 5.3 and newer}}{{18-19=, y: }}{{26-26=)}}

  // No warning; this is matching the last parameter.
  ambiguous1(b: 3) { $0 }
}

// Ambiguity with two unlabeled arguments.
func ambiguous2( // expected-note{{'ambiguous2(_:a:y:b:_:x:)' contains defaulted closure parameters '_' and '_'}}
  _: (Int) -> Int = { $0 },
  a: Int = 5,
  y: (Int) -> Int = { $0 },
  b: Int = 5,
  _: (Int) -> Int = { $0 },
  x: Int = 5
) {}

func testAmbiguous2() {
    ambiguous2 { $0 } // expected-warning{{since Swift 5.3, unlabeled trailing closure argument matches earlier parameter '_' rather than later parameter with the same name}}
}

// Ambiguity with one unlabeled argument.
func ambiguous3( // expected-note{{'ambiguous3(x:a:y:b:_:c:)' contains defaulted closure parameters 'x' and '_'}}
  x: (Int) -> Int = { $0 },
  a: Int = 5,
  y: (Int) -> Int = { $0 },
  b: Int = 5,
  _: (Int) -> Int = { $0 },
  c: Int = 5
) {}

func testAmbiguous3() {
  ambiguous3 { $0 } // expected-warning{{since Swift 5.3, unlabeled trailing closure argument matches parameter 'x' rather than parameter '_'}}
    // expected-note@-1{{label the argument with '_' to retain the pre-Swift 5.3 behavior}}{{13-13=(}}{{20-20=)}}
    // expected-note@-2{{label the argument with 'x' to silence this warning for Swift 5.3 and newer}}{{13-13=(x: }}{{20-20=)}}
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
func notAmbiguous3( // expected-note{{'notAmbiguous3(x:a:y:b:_:c:)' declared here}}
  x: (Int) -> Int = { $0 },
  a: Int = 5,
  y: (Int) -> Int = { $0 },
  b: Int = 5,
  _: (Int) -> Int ,
  c: Int = 5
) { }

func testNotAmbiguous3() {
  notAmbiguous3 { $0 } // expected-warning{{backward matching of the unlabeled trailing closure is deprecated; label the argument with '_' to suppress this warning}}
}
