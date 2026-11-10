// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/52877

protocol Brew { // expected-cparser-note {{in declaration of 'Brew'}}
  tripel() -> Int // expected-cparser-error {{expected 'func' keyword in instance method declaration}} {{3-3=func }} expected-astgen-error {{unexpected code before modifier}}

  quadrupel: Int { get } // expected-cparser-error {{expected 'var' keyword in property declaration}} {{3-3=var }}

  static + (lhs: Self, rhs: Self) -> Self // expected-cparser-error {{expected 'func' keyword in operator function declaration}} {{10-10=func }} expected-astgen-error {{expected 'func' in function}}

  * (lhs: Self, rhs: Self) -> Self // expected-cparser-error {{expected 'func' keyword in operator function declaration}} {{3-3=func }}
                                   // expected-cparser-error @-1 {{operator '*' declared in protocol must be 'static'}} {{3-3=static }} expected-astgen-error@-1 {{expected 'func' in function}}

  ipa: Int { get }; apa: Float { get }
  // expected-cparser-error @-1 {{expected 'var' keyword in property declaration}} {{3-3=var }} expected-astgen-error@-1 {{expected 'var' in variable}}
  // expected-cparser-error @-2 {{expected 'var' keyword in property declaration}} {{21-21=var }} expected-astgen-error@-2 {{expected 'var' in variable}}

  stout: Int { get } porter: Float { get }
  // expected-cparser-error @-1 {{expected 'var' keyword in property declaration}} {{3-3=var }}
  // expected-cparser-error @-2 {{expected declaration}} expected-astgen-error@-2 {{expected 'var' in variable}}
  // expected-cparser-error @-3 {{consecutive declarations on a line must be separated by ';'}} expected-astgen-error@-3 {{expected 'var' in variable}}
}

infix operator %%

struct Bar {
  fisr = 0x5F3759DF // expected-cparser-error {{expected 'var' keyword in property declaration}} {{3-3=var }} expected-astgen-error {{expected 'var' in variable}}

  %%<T: Brew> (lhs: T, rhs: T) -> T { // expected-cparser-error {{expected 'func' keyword in operator function declaration}} {{3-3=func }}
                                      // expected-cparser-error @-1 {{operator '%%' declared in type 'Bar' must be 'static'}}
                                      // expected-cparser-error @-2 {{member operator '%%' must have at least one argument of type 'Bar'}} expected-astgen-error@-2 {{expected 'func' in function}}
    lhs + lhs + rhs + rhs
  }

  _: Int = 42 // expected-cparser-error {{expected 'var' keyword in property declaration}} {{3-3=var }} expected-astgen-error {{expected 'var' in variable}}
              // expected-error @-1 {{property declaration does not bind any variables}}

  (light, dark) = (100, 200)// expected-cparser-error {{expected 'var' keyword in property declaration}} {{3-3=var }} expected-astgen-error {{expected 'var' in variable}}
  
  a, b: Int // expected-cparser-error {{expected 'var' keyword in property declaration}} {{3-3=var }} expected-astgen-error {{expected 'var' in variable}}
}

class Baz {

  instanceMethod() {} // expected-cparser-error {{expected 'func' keyword in instance method declaration}} {{3-3=func }} expected-astgen-error {{unexpected code 'instanceMethod() {}' before modifier}}

  static staticMethod() {} // expected-cparser-error {{expected 'func' keyword in static method declaration}} {{10-10=func }} expected-astgen-error {{unexpected code before modifier}} expected-astgen-error {{'static' cannot appear after another 'static' or 'class' keyword}}

  instanceProperty: Int { 0 } // expected-cparser-error {{expected 'var' keyword in property declaration}} {{3-3=var }}

  static staticProperty: Int { 0 } // expected-cparser-error {{expected 'var' keyword in static property declaration}} {{10-10=var }} expected-astgen-error {{expected 'var' in variable}}
}

class C1 {
  class classMethod() {} // expected-cparser-error {{expected '{' in class}} expected-astgen-error {{unexpected code '()' in class}}
}

class C2 {
  class classProperty: Int { 0 } // expected-error {{inheritance from non-protocol, non-class type 'Int'}}
                                 // expected-cparser-note @-1 {{in declaration of 'classProperty'}}
                                 // expected-cparser-error @-2 {{expected declaration}} expected-astgen-error@-2 {{unexpected code '0' in class}}
}
