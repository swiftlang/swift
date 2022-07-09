// expected-note{{top-level code defined in this source file}}
// expected-note@-1{{pass '-parse-as-library' to compiler invocation if this is intentional}}
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s %S/delegate.swift

// Serialized partial AST support:
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module-path %t.swiftmodule -primary-file %s %S/delegate.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library -typecheck %t.swiftmodule -primary-file %S/delegate.swift -verify

// REQUIRES: objc_interop

hi()
