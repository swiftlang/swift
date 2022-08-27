// RUN: %target-typecheck-verify-swift

func foo() {}
func bar() {}

if #_hasSymbol(foo) {}
if #_hasSymbol(foo), #_hasSymbol(bar) {}
guard #_hasSymbol(foo) else { fatalError() }
while #_hasSymbol(foo) {}

if #_hasSymbol(foo) == false {} // expected-error {{expected '{' after 'if' condition}}

_ = #_hasSymbol(foo) {} // expected-error {{#_hasSymbol may only be used as condition of}}

(#_hasSymbol(foo) ? 1 : 0) // expected-error {{#_hasSymbol may only be used as condition of}}

if !#_hasSymbol(foo) {} // expected-error {{#_hasSymbol may only be used as condition of}}

if let _ = Optional(5), !#_hasSymbol(foo) {} {} // expected-error {{#_hasSymbol may only be used as condition of}}

if #_hasSymbol {} // expected-error {{expected '(' in #_hasSymbol directive}}

// expected-error@+3 {{expected ')' in #_hasSymbol condition}}
// expected-error@+2 {{expected '{' after 'if' condition}}
// expected-note@+1 {{to match this opening '('}}
if #_hasSymbol(struct) {} // expected-error {{expected expression in #_hasSymbol}}

// expected-error@+3 {{expected ')' in #_hasSymbol condition}}
// expected-error@+2 {{expected '{' after 'if' condition}}
// expected-note@+1 {{to match this opening '('}}
if #_hasSymbol(foo bar) {}

// expected-error@+2 {{expected ')' in #_hasSymbol condition}}
// expected-note@+1 {{to match this opening '('}}
if #_hasSymbol(foo {}

