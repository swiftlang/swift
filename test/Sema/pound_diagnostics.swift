// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -c -verify %s -o /dev/null

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

#error(test 123) // expected-error {{expected string literal in #error directive}}{{8-8="}}{{16-16="}}

#error test 123 // expected-error {{expected string literal in #error directive}}{{8-8=("}}{{16-16=")}}

#error "no parentheses error" // expected-error {{#error directive requires parentheses}}{{8-8=(}}{{30-30=)}}
#warning "no parentheses warning" // expected-error {{#warning directive requires parentheses}}{{10-10=(}}{{34-34=)}}

#error "left parentheses error") // expected-error {{expected '(' in #error directive}}{{8-8=(}}
#warning("right parentheses warning" // expected-error {{expected ')' in #warning directive}}{{37-37=)}}

#error("interp\("olation")") // expected-error {{string interpolation is not allowed in #error directive}}
#warning("interp\("olation")") // expected-error {{string interpolation is not allowed in #warning directive}}

#error("extra tokens") var i = 0 // expected-error {{extra tokens following #error directive}}
#warning("extra tokens") var j = 0 // expected-error {{extra tokens following #warning directive}}

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

  switch 34 {
  #warning("warnings can be nested in switch statements") // expected-warning {{warnings can be nested in switch statements}}
  #if true
  #error("errors can be nested in if-configs inside switch statements too") // expected-error {{errors can be nested in if-configs inside switch statements too}}
  case 5:
  #warning("way too many levels of nesting") // expected-warning {{way too many levels of nesting}}
  #elseif false
  #error("this still shouldn't trip")
  #endif
  default: break
  }
}

public // expected-error @+1 {{expected declaration}}
#warning("public warning") // expected-warning {{public warning}}
func bar() {}

class C { // expected-note {{in declaration of 'C'}}
  private // expected-error @+1 {{expected declaration}}
  #error("private error") // expected-error  {{private error}}
  func bar() {}
}
