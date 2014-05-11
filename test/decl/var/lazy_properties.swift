// RUN: %swift -parse %s -verify

@lazy func lazy_func() {} // expected-error {{'lazy' attribute may only be used on 'var' declarations}}

//@lazy let b = 42  // error in libraries.

func test() {
  @lazy var a = 42

  @lazy let b = 42  // expected-error {{'lazy' attribute cannot be used on a let}}

  @lazy var c : Int { return 42 } // expected-error {{'lazy' attribute may not be used on a computed property}}



}


