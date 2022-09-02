// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/overlaydir)
// RUN: %empty-directory(%t/frameworks)

// RUN: cp -rf %S/Inputs/frameworks/Simple.framework %t/frameworks/

// RUN: echo "current-version: 1830.100" > %t/frameworks/Simple.framework/Simple.tbd
// RUN: echo "@_exported import Simple" > %t.overlay.swift
// RUN: echo "public func additional() {}" >> %t.overlay.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -disable-implicit-concurrency-module-import -module-name Simple -F %t/frameworks/ %t.overlay.swift -emit-module-path %t/overlaydir/Simple.swiftmodule
// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/overlaydir/ -F %t/frameworks

import Simple

func canImportVersioned() {
#if canImport(Simple, _underlyingVersion: 3.3)
  let a = 1  // expected-warning {{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Simple, _underlyingVersion: 1830.100)
  let b = 1  // expected-warning {{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Simple, _underlyingVersion: 1830.11)
  let c = 1  // expected-warning {{initialization of immutable value 'c' was never used; consider replacing with assignment to '_' or removing it}}
#endif
}

func canNotImportVersioned() {
#if canImport(Simple, _underlyingVersion: 1831)
  let a = 1
#endif

#if canImport(Simple, _underlyingVersion: 1830.101)
  let b = 1
#endif
}
