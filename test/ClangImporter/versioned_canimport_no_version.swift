// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/overlaydir)
// RUN: %empty-directory(%t/frameworks)

// RUN: cp -rf %S/Inputs/frameworks/Simple.framework %t/frameworks/

// RUN: echo "" > %t/frameworks/Simple.framework/Simple.tbd
// RUN: echo "@_exported import Simple" > %t.overlay.swift
// RUN: echo "public func additional() {}" >> %t.overlay.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -disable-implicit-concurrency-module-import -module-name Simple -F %t/frameworks/ %t.overlay.swift -emit-module-path %t/overlaydir/Simple.swiftmodule
// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/overlaydir/ -F %t/frameworks

import Simple

func canImportVersioned() {
#if canImport(Simple, _underlyingVersion: 3.3) // expected-warning {{cannot find user version number for Clang module 'Simple'; version number ignored}}
#endif
}
