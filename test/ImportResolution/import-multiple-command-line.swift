// RUN: %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -sdk "" -verify
// RUN: %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -sdk "" -import-module abcde -import-module aeiou

// RUN: not %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -sdk "" -import-module abcde -import-module aeiou -import-module 3333 2>&1 | %FileCheck -check-prefix=NON-IDENT %s

// NON-IDENT: error: module name "3333" is not a valid identifier

var c: C? // expected-error {{use of undeclared type 'C'}}
var u: U? // expected-error {{use of undeclared type 'U'}}
var qA1: abcde.A? // expected-error {{use of undeclared type 'abcde'}}
var qA2: aeiou.A? // expected-error {{use of undeclared type 'aeiou'}}
