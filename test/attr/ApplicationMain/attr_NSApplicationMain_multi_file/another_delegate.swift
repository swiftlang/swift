// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s %S/delegate.swift

// Serialized partial AST support:
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name main -emit-module-path %t.swiftmodule -primary-file %s %S/delegate.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name main -parse-as-library -typecheck %t.swiftmodule -primary-file %S/delegate.swift -verify -verify-ignore-unknown

// REQUIRES: objc_interop

import AppKit

@NSApplicationMain // expected-error{{'NSApplicationMain' attribute can only apply to one class in a module}}
class EvilDelegate: NSObject, NSApplicationDelegate {
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected error produced: 'NSApplicationMain' attribute can only apply to one class in a module
