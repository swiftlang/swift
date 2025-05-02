// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s %S/delegate.swift

// Serialized partial AST support:
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name main -emit-module-path %t.swiftmodule -primary-file %s %S/delegate.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name main -parse-as-library -typecheck %t.swiftmodule -primary-file %S/delegate.swift -verify -verify-ignore-unknown

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name main -parse-as-library -typecheck -enable-upcoming-feature DeprecateApplicationMain %t.swiftmodule -primary-file %S/delegate.swift -verify -verify-ignore-unknown -verify-additional-prefix deprecated-

// REQUIRES: objc_interop
// REQUIRES: swift_feature_DeprecateApplicationMain

import AppKit

@NSApplicationMain // expected-error{{'NSApplicationMain' attribute can only apply to one class in a module}}
// expected-deprecated-warning@-1 {{'NSApplicationMain' is deprecated; this is an error in the Swift 6 language mode}}
// expected-deprecated-note@-2 {{use '@main' instead}} {{1-19=@main}}
class EvilDelegate: NSObject, NSApplicationDelegate {
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected error produced: 'NSApplicationMain' attribute can only apply to one class in a module
