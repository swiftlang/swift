// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

import _PointerBounds

let countedBy = PointerParam.countedBy(pointer: 1, count: "len")
// expected-error@+1{{expected PointerParam enum literal as argument, got 'countedBy'}}
@PointerBounds(countedBy)
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: String) {
}
