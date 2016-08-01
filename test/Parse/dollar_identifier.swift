// RUN: %target-parse-verify-swift

// SR-1661: Dollar was accidentally allowed as an identifier and identifier head.

func dollarVar() {
  var $ : Int = 42 // expected-error {{expected numeric value following '$'}} expected-error {{expected pattern}}
}
func dollarLet() {
  let $ = 42 // expected-error {{expected numeric value following '$'}} expected-error {{expected pattern}} 
}
func dollarClass() {
  class $ {} // expected-error {{expected numeric value following '$'}} 
             // expected-error@-1 {{expression resolves to an unused function}}
             // expected-error@-2 {{expected identifier in class declaration}}
             // expected-error@-3 {{braced block of statements is an unused closure}}
}
func dollarEnum() {
  enum $ {} // expected-error {{expected numeric value following '$'}} 
            // expected-error@-1 {{expected identifier in enum declaration}}
            // expected-error@-2 {{expression resolves to an unused function}}
            // expected-error@-3 {{braced block of statements is an unused closure}}
}
func dollarStruct() {
  struct $ {} // expected-error {{expected numeric value following '$'}}
              // expected-error@-1 {{expected identifier in struct declaration}}
              // expected-error@-2 {{braced block of statements is an unused closure}}
              // expected-error@-3 {{expression resolves to an unused function}}
}

