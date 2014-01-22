// RUN: %swift -parse %s -verify

// DynamicSelf is only allowed on the return type of class and
// protocol methods.
func global() -> DynamicSelf { } // expected-error{{global function cannot return 'DynamicSelf'}}

func inFunction() {
  func local() -> DynamicSelf { } // expected-error{{local function cannot return 'DynamicSelf'}}
}

struct S0 {
  func f() -> DynamicSelf { } // expected-error{{struct method cannot return 'DynamicSelf'; did you mean to use the struct type 'S0'?}}{{15-26=S0}}

  func g(ds: DynamicSelf) { } // expected-error{{'DynamicSelf' can only be introduced in the return type of a method; did you mean 'S0'?}}{{14-25=S0}}
}

enum E0 {
  func f() -> DynamicSelf { } // expected-error{{enum method cannot return 'DynamicSelf'; did you mean to use the enum type 'E0'?}}{{15-26=E0}}

  func g(ds: DynamicSelf) { } // expected-error{{'DynamicSelf' can only be introduced in the return type of a method; did you mean 'E0'?}}{{14-25=E0}}
}

class C0 {
  func f() -> DynamicSelf { } // okay

  func g(ds: DynamicSelf) { } // expected-error{{'DynamicSelf' can only be introduced in the return type of a method; did you mean 'C0'?}}{{14-25=C0}}
}

protocol P0 {
  func f() -> DynamicSelf { } // okay

  func g(ds: DynamicSelf) { } // expected-error{{'DynamicSelf' can only be introduced in the return type of a method; did you mean 'Self'?}}{{14-25=Self}}
}
