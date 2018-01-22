// RUN: %target-typecheck-verify-swift

#warning("this should be a warning") // expected-warning {{this should be a warning}}
#error("this should be an error") // expected-error {{this should be an error}}

#if false
#error("this shouldn't cause any errors")
#endif

#if true
#error("this should also be an error") // expected-error {{this should also be an error}}
#endif

#error() // expected-error {{expected string literal in #error directive}}
#warning() // expected-error {{expected string literal in #warning directive}}

#error "no parentheses error" // expected-error {{expected '(' in #error directive}}
#warning "no parentheses warning" // expected-error {{expected '(' in #warning directive}}

#error("interp\("olation")") // expected-error {{string interpolation is not allowed in #error directive}}
#warning("interp\("olation")") // expected-error {{string interpolation is not allowed in #warning directive}}

class RegularClass {
  #error("errors can be nested in classes") // expected-error {{errors can be nested in classes}}
}

struct RegularStruct {
  #error("errors can be nested in structs") // expected-error {{errors can be nested in structs}}
}

protocol RegularProtocol {
  #warning("warnings can be nested in protocols") // expected-warning {{warnings can be nested in protocols}}
}

extension RegularClass {
  #error("errors can be nested in extensions") // expected-error {{errors can be nested in extensions}}
}

func foo() {
  #error("errors can be nested in functions") // expected-error {{errors can be nested in functions}}
}
