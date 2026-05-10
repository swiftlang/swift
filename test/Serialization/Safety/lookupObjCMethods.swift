// RUN: %target-swift-frontend -typecheck -verify %s \
// RUN:   -enable-deserialization-safety \
// RUN:   -Xllvm -debug-only=Serialization 2>&1 | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: asserts

import Foundation

// Fails at reading __SwiftNativeNSEnumerator.init() on macOS.
NSString.instancesRespond(to: "init") // expected-warning {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}
// CHECK: Skipping unsafe deserialization: 'init()'
