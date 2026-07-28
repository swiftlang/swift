// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/frameworks)
// RUN: %empty-directory(%t/overlaydir)

// RUN: cp -rf %S/Inputs/frameworks/Simple.framework %t/frameworks/

// Creates a framework with a Clang module and a .tbd as well as a Swift
// overlay. The .tbd has a version but the .swiftmodule was built without a
// user module version. A `_version` query cannot find a user version and the
// version is ignored, but because the underlying Clang module does carry a
// version a note points at `_underlyingVersion`, which honors the `.tbd`
// version.

// RUN: sed -i -e "s/1830\.100/3/g" %t/frameworks/Simple.framework/Simple.tbd
// RUN: echo "@_exported import Simple" > %t.overlay.swift
// RUN: echo "public func additional() {}" >> %t.overlay.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -disable-implicit-concurrency-module-import -module-name Simple -F %t/frameworks/ %t.overlay.swift -emit-module-path %t/overlaydir/Simple.swiftmodule
// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/overlaydir/ -F %t/frameworks

import Simple

func canImportVersion() {
#if canImport(Simple, _version: 3) // expected-warning {{cannot find user version number for module 'Simple'; version number ignored}} expected-note {{the underlying Clang module 'Simple' has version 3.0.0; use '_underlyingVersion' to check it}}
  let a = 1  // expected-warning {{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
#endif
}

func canImportUnderlyingVersion() {
#if canImport(Simple, _underlyingVersion: 3)
  let a = 1  // expected-warning {{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Simple, _underlyingVersion: 4)
  let b = 1
#endif
}
