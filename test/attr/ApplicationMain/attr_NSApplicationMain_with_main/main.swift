// expected-note{{top-level code defined in this source file}}
// RUN: %target-swift-frontend %clang-importer-sdk -parse -verify %s %S/delegate.swift

// Serialized partial AST support:
// RUN: %target-swift-frontend %clang-importer-sdk -emit-module-path %t.swiftmodule -primary-file %s %S/delegate.swift
// RUN: %target-swift-frontend %clang-importer-sdk -parse-as-library -parse %t.swiftmodule -primary-file %S/delegate.swift -verify

hi()
