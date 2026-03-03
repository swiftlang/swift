// RUN: %target-typecheck-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

import Foundation
import CoreGraphics

func foo(_ x: Double) {}
func bar() -> Double { 0 }

// https://github.com/swiftlang/swift/issues/78376
let _: (CGFloat) -> Void = foo
// expected-error@-1 {{cannot convert value of type '(Double) -> ()' to specified type '(CGFloat) -> Void'}}
let _: () -> CGFloat = bar
// expected-error@-1 {{cannot convert value of type '() -> Double' to specified type '() -> CGFloat'}}
