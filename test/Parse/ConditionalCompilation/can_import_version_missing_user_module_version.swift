// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/textual)
// RUN: %empty-directory(%t/binary)

// RUN: echo "public func foo() {}" > %t/Foo.swift

// RUN: %target-swift-frontend -emit-module %t/Foo.swift -module-name NoUserModuleVersion -swift-version 5 -disable-implicit-concurrency-module-import -emit-module-interface-path %t/textual/NoUserModuleVersion.swiftinterface -enable-library-evolution -emit-module-path %t/binary/NoUserModuleVersion.swiftmodule

// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/textual
// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/binary

import NoUserModuleVersion

func testCanImportNoUserModuleVersion() {

#if canImport(NoUserModuleVersion, _version: 113.331) // expected-warning {{cannot find user version number for module 'NoUserModuleVersion'; version number ignored}}
  // NOTE: Duplicate warning because the canImport request happens twice when parser
  // validation is enabled.
  // TODO(ParserValidation): expected-warning@-3 *{{cannot find user version number for module 'NoUserModuleVersion'; version number ignored}}
  let a = 1 // expected-warning {{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(NoUserModuleVersion, _version: 2) // expected-warning {{cannot find user version number for module 'NoUserModuleVersion'; version number ignored}}
  // NOTE: Duplicate warning because the canImport request happens twice when parser
  // validation is enabled.
  // TODO(ParserValidation): expected-warning@-3 *{{cannot find user version number for module 'NoUserModuleVersion'; version number ignored}}
  let b = 1 // expected-warning {{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
#endif

}
