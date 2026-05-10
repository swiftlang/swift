// RUN: %empty-directory(%t)

// Creates a module with a fake binary swiftmodule containing bogus data. Both
// the swiftinterface and the binary module will be visible to the compiler
// when it loads 'BadBinaryModule'. The compiler should prefer the user module
// version printed in the swiftinterface and ignore the binary module.

// RUN: echo "public func foo() {}" > %t/Foo.swift
// RUN: %target-swift-frontend -typecheck %t/Foo.swift -module-name BadBinaryModule -swift-version 5 -disable-implicit-concurrency-module-import -user-module-version 2 -emit-module-interface-path %t/BadBinaryModule.swiftinterface -enable-library-evolution
// RUN: echo 'none' > %t/BadBinaryModule.swiftmodule

// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/

import BadBinaryModule

func testCanImportVersion() {
#if canImport(BadBinaryModule, _version: 2)
  let badBinaryModuleVersionMatch = 1 // expected-warning {{initialization of immutable value 'badBinaryModuleVersionMatch' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(BadBinaryModule, _version: 3)
  let badBinaryModuleVersionTooHigh = 1
#endif
}
