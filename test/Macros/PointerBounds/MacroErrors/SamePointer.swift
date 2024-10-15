// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir

import _PointerBounds

// expected-error@+1{{multiple PointerParams referring to parameter with index 1: .countedBy(pointer: 1, count: "dummy", nonescaping: false) and .countedBy(pointer: 1, count: "len", nonescaping: false)}}
@PointerBounds(.countedBy(pointer: 1, count: "len"), .countedBy(pointer: 1, count: "dummy"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt, _ dummy: CInt) {
}
