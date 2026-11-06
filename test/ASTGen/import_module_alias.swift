// Importing the real name of an aliased module is diagnosed by both parsers.

// RUN: %target-typecheck-verify-swift -module-alias Foo=Swift
// RUN: %target-typecheck-verify-swift -module-alias Foo=Swift \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

import Swift // expected-error {{cannot refer to module as 'Swift' because it has been aliased; use 'Foo' instead}} {{8-13=Foo}}
