// RUN: %empty-directory(%t.mcp)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -typecheck -verify -verify-ignore-unrelated %s -module-cache-path %t.mcp 2>&1
// | %FileCheck %s

// REQUIRES: objc_interop

import RawIdentifierSwiftNames

func testRawIdentifiers() {
  let t = RawIdentifierTest()

  t.`do something`(`with value`: 0) // expected-error {{incorrect argument label in call (have '`with value`:', expected '`with value `:')}}
  t.`do something`(`with value `: 0)
  t.methodWithRawIdentifier(0) // expected-error {{'methodWithRawIdentifier' has been renamed to '`do something`(`with value `:)'}}

  _ = t.`raw prop`
  _ = t.rawIdentifierProp // expected-error {{'rawIdentifierProp' has been renamed to '`raw prop`'}}

  _ = `test raw`
  _ = test_raw // expected-error {{'test_raw' has been renamed to '`test raw`'}}

  _ = `3global raw`
  _ = global_raw // expected-error {{'global_raw' has been renamed to '`3global raw`'}}

  _ = `foo`
  _ = foo
  _ = fooWithUnnecessaryBackticks // expected-error {{'fooWithUnnecessaryBackticks' has been renamed to 'foo'}}
}
