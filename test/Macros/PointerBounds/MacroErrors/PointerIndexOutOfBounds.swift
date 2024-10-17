// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

import _PointerBounds

// expected-error@+1{{pointer index out of bounds}}
@PointerBounds(.countedBy(pointer: 3, count: "len"))
// expected-note@+1{{function myFunc has parameter indices 1..2}}
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt) {
}
// expected-error@+1{{pointer index out of bounds}}
@PointerBounds(.countedBy(pointer: 0, count: "len"))
// expected-note@+1{{function myFunc2 has parameter indices 1..2}}
func myFunc2(_ ptr: UnsafePointer<CInt>, _ len: CInt) {
}
// expected-error@+1{{pointer index out of bounds}}
@PointerBounds(.countedBy(pointer: 0, count: "1"))
// expected-note@+1{{function myFunc3 has no parameters}}
func myFunc3() {
}
