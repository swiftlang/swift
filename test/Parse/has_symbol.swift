// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Library.swiftmodule -parse-as-library %t/Library.swift -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t

// UNSUPPORTED: OS=windows-msvc

//--- Library.swift

public func foo() {}
public func bar() {}

//--- Client.swift

@_weakLinked import Library

if #_hasSymbol(foo) {}
if #_hasSymbol(foo), #_hasSymbol(bar) {}
guard #_hasSymbol(foo) else { fatalError() }
while #_hasSymbol(foo) {}

if #_hasSymbol(foo) == false {} // expected-cparser-error {{expected '{' after 'if' condition}} expected-astgen-error {{#_hasSymbol may only be used as condition of an 'if', 'guard' or 'while' statement}}

_ = #_hasSymbol(foo) {} // expected-error {{#_hasSymbol may only be used as condition of}}

(#_hasSymbol(foo) ? 1 : 0) // expected-error {{#_hasSymbol may only be used as condition of}}

if !#_hasSymbol(foo) {} // expected-error {{#_hasSymbol may only be used as condition of}}

if let _ = Optional(5), !#_hasSymbol(foo) {} {} // expected-error {{#_hasSymbol may only be used as condition of}}

if #_hasSymbol {} // expected-error {{expected '(' in #_hasSymbol directive}}

// expected-cparser-error@+3 {{expected ')' in #_hasSymbol condition}}
// expected-cparser-error@+2 {{expected '{' after 'if' condition}} expected-astgen-error@+2 {{expected identifier in struct}} expected-astgen-error@+2 {{unexpected code ')' in struct}} expected-astgen-error@+2 {{expected value and ')' to end macro expansion}} expected-astgen-error@+2 {{expected '{' in 'if' statement}}
// expected-cparser-note@+1 {{to match this opening '('}}
if #_hasSymbol(struct) {} // expected-cparser-error {{expected expression in #_hasSymbol}}

// expected-cparser-error@+3 {{expected ')' in #_hasSymbol condition}}
// expected-cparser-error@+2 {{expected '{' after 'if' condition}} expected-astgen-error@+2 {{unexpected code 'bar' in macro expansion}}
// expected-cparser-note@+1 {{to match this opening '('}}
if #_hasSymbol(foo bar) {}

// expected-cparser-error@+2 {{expected ')' in #_hasSymbol condition}} expected-astgen-error@+2 {{extra trailing closure passed in call}} expected-astgen-error@+2 {{expected ')' to end macro expansion}} expected-astgen-error@+2 {{expected body in 'if' statement}} expected-astgen-error@+2 {{expected '}' to end 'if' statement}}
// expected-note@+1 {{to match this opening '('}}
if #_hasSymbol(foo {}

