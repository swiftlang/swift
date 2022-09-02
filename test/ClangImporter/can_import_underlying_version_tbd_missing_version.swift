// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/frameworks)

// RUN: cp -rf %S/Inputs/frameworks/Simple.framework %t/frameworks/

// RUN: echo "" > %t/frameworks/Simple.framework/Simple.tbd
// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -F %t/frameworks

import Simple

func canImportVersioned() {
#if canImport(Simple, _underlyingVersion: 3.3) // expected-warning {{cannot find user version number for Clang module 'Simple'; version number ignored}}
  let a = 1  // expected-warning {{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
#endif
}
