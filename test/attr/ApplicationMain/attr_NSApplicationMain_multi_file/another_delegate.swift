// RUN: %target-swift-frontend %clang-importer-sdk -parse -verify %s %S/delegate.swift

// Serialized partial AST support:
// RUN: %target-swift-frontend %clang-importer-sdk -module-name main -emit-module-path %t.swiftmodule -primary-file %s %S/delegate.swift
// RUN: %target-swift-frontend %clang-importer-sdk -module-name main -parse-as-library -parse %t.swiftmodule -primary-file %S/delegate.swift -verify

import AppKit

@NSApplicationMain // expected-error{{'NSApplicationMain' attribute can only apply to one class in a module}}
class EvilDelegate: NSObject, NSApplicationDelegate {
}
