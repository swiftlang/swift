// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

import _PointerBounds

let pointerIndex = 1
// expected-error@+1{{expected integer literal, got 'pointerIndex'}}
@PointerBounds(.countedBy(pointer: pointerIndex, count: "len"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: String) {
}
