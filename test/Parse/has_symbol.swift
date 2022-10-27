// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Library.swiftmodule -parse-as-library %t/Library.swift -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t

// REQUIRES: VENDOR=apple

//--- Library.swift

public func foo() {}
public func bar() {}

//--- Client.swift

@_weakLinked import Library

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

