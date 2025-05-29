// RUN: %target-swift-frontend -parse-as-library -emit-sil %s -o /dev/null -verify
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: swift_in_compiler
// REQUIRES: objc_interop, OS=macosx

import Foundation

@_noObjCBridging
func useOfExistentialNoObjc() -> NSArray.Type {
  NSArray.self // expected-error{{calls of Objective-C methods can have unpredictable performance}}
}