// RUN: %target-swift-frontend -typecheck -verify %s %S/main1.swift

// Serialized partial AST support:
// RUN: %target-swift-frontend -module-name main -emit-module-path %t.swiftmodule -primary-file %s %S/main1.swift
// RUN: %target-swift-frontend -module-name main -parse-as-library -typecheck %t.swiftmodule -primary-file %S/main1.swift -verify -verify-ignore-unknown

class Main {
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected error produced: 'main' attribute can only apply to one class in a module
