// expected-note{{top-level code defined in this source file}}
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s %S/delegate.swift

// REQUIRES: objc_interop

hi()
