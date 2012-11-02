// RUN: %swift %s -parse -verify
println(a) // expected-error {{use of unresolved identifier 'a'}}
