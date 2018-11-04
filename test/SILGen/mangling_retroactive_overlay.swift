// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays
// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) %s -enable-sil-ownership | %FileCheck %s
// REQUIRES: objc_interop

import ObjectiveC

struct RequiresEq<T: Equatable> { }

// Note: the NSObject: Equatable conformance from the overlay is not considered
// to be a "retroactive" conformance, so ensure that it isn't mangled as such.
// CHECK: sil hidden @$s28mangling_retroactive_overlay4testyyAA10RequiresEqVySo8NSObjectCGF 
func test(_: RequiresEq<NSObject>) { }

