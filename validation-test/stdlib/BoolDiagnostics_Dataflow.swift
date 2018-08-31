// RUN: %target-swift-frontend -emit-sil -verify %s

@_silgen_name("opaque")
func opaque() -> Int

func test_constantFoldAnd1() -> Int {
  while true && true {
    return 42
  }
  // FIXME: this is a false positive.
} // expected-error {{missing return in a function expected to return 'Int'}}

func test_constantFoldAnd2() -> Int {
  while true && false {
    return 42
  }
} // expected-error {{missing return in a function expected to return 'Int'}}

func test_constantFoldAnd3() -> Int {
  while false && true {
    return 42
  }
} // expected-error {{missing return in a function expected to return 'Int'}}

func test_constantFoldAnd4() -> Int {
  while false && false {
    return 42
  }
} // expected-error {{missing return in a function expected to return 'Int'}}

func test_constantFoldOr1() -> Int {
  while true || true {
    return 42
  }
  // FIXME: this is a false positive.
} // expected-error {{missing return in a function expected to return 'Int'}}

func test_constantFoldOr2() -> Int {
  while true || false {
    return 42
  }
  // FIXME: this is a false positive.
} // expected-error {{missing return in a function expected to return 'Int'}}

func test_constantFoldOr3() -> Int {
  while false || true {
    return 42
  }
  // FIXME: this is a false positive.
} // expected-error {{missing return in a function expected to return 'Int'}}

func test_constantFoldOr4() -> Int {
  while false || false {
    return 42
  }
} // expected-error {{missing return in a function expected to return 'Int'}}

// expected-warning@+1 {{'ExpressibleByStringInterpolation' is deprecated: it will be replaced or redesigned in Swift 5.0.  Instead of conforming to 'ExpressibleByStringInterpolation', consider adding an 'init(_:String)'}}
typealias X = ExpressibleByStringInterpolation
