// RUN: %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -sdk "" -verify
// RUN: %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -sdk "" -import-module abcde -import-module aeiou

// RUN: not %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -sdk "" -import-module abcde -import-module aeiou -import-module 3333 2>&1 | %FileCheck -check-prefix=NON-IDENT %s

// NON-IDENT: error: module name "3333" is not a valid identifier

var c: C? // expected-error {{cannot find type 'C' in scope}}
var u: U? // expected-error {{cannot find type 'U' in scope}}
var qA1: abcde.A? // expected-error {{cannot find type 'abcde' in scope}}
var qA2: aeiou.A? // expected-error {{cannot find type 'aeiou' in scope}}
