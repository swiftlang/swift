// RUN: %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -sdk "" -verify
// RUN: %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -sdk "" -import-module abcde

// RUN: not %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -sdk "" -import-module 3333 2>&1 | %FileCheck -check-prefix=NON-IDENT %s
// NON-IDENT: error: module name "3333" is not a valid identifier

// RUN: not %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -sdk "" -import-module NON_EXISTENT 2>&1 | %FileCheck -check-prefix=NON-EXISTENT %s
// NON-EXISTENT: error: no such module 'NON_EXISTENT'

// RUN: not %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -sdk "" -testable-import-module abcde 2>&1 | %FileCheck -check-prefix=NON-TESTABLE %s
// NON-TESTABLE: error: module 'abcde' was not compiled for testing

var a: A? // expected-error {{cannot find type 'A' in scope}}
var qA: abcde.A? // expected-error {{cannot find type 'abcde' in scope}}
