// expected-note{{top-level code defined in this source file}}
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s %S/delegate.swift

// Serialized partial AST support:
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module-path %t.swiftmodule -primary-file %s %S/delegate.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library -typecheck %t.swiftmodule -primary-file %S/delegate.swift -verify

// REQUIRES: objc_interop

hi()
