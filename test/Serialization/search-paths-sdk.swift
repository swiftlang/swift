// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mock-sdk/usr/lib/swift/SystemLibrary.swiftmodule)
// RUN: %empty-directory(%t/secret)

// RUN: %target-swift-frontend -emit-module %S/../Inputs/empty.swift -o %t/secret/SecretLibrary.swiftmodule
// RUN: %target-swift-frontend -emit-module %S/../Inputs/empty.swift -o %t/mock-sdk/usr/lib/swift/SystemLibrary.swiftmodule/%target-swiftmodule-name -module-name SystemLibrary -I %t/secret -serialize-debugging-options

// We pick up search paths from normal imports...
// RUN: %target-swift-frontend -typecheck %s -I %t/mock-sdk/usr/lib/swift/

// We do not pick up search paths from any imports in Swift 6 mode
// RUN: %target-swift-frontend -typecheck %s -I %t/mock-sdk/usr/lib/swift/ -verify -show-diagnostics-after-fatal -swift-version 6

// ...but not from content in the SDK.
// RUN: %target-swift-frontend -typecheck %s -sdk %t/mock-sdk -verify -show-diagnostics-after-fatal

import SystemLibrary
import SecretLibrary // expected-error {{no such module 'SecretLibrary'}}
