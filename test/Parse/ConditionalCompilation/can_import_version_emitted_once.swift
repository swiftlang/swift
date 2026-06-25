// Regression test: cannot_find_module_version is emitted exactly once per
// `#if canImport(M, _version: v)` for a versionless module. Without a count
// marker the verifier requires exactly one match.
//
// Both the textual and binary swiftmodule paths are exercised. This test runs
// without parser validation enabled, demonstrating that the duplicate is not
// specific to parser validation.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/textual)
// RUN: %empty-directory(%t/binary)
// RUN: echo "public func foo() {}" > %t/Foo.swift

// RUN: %target-swift-frontend -emit-module %t/Foo.swift \
// RUN:   -module-name NoUserModuleVersion -swift-version 5 \
// RUN:   -disable-implicit-concurrency-module-import \
// RUN:   -emit-module-interface-path %t/textual/NoUserModuleVersion.swiftinterface \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/binary/NoUserModuleVersion.swiftmodule

// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/textual
// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/binary

import NoUserModuleVersion

func testCanImportNoUserModuleVersion() {

#if canImport(NoUserModuleVersion, _version: 113.331) // expected-warning {{cannot find user version number for module 'NoUserModuleVersion'; version number ignored}}
  let a = 1 // expected-warning {{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
#endif

}
