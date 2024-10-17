// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

import _PointerBounds

// expected-error@+1{{no parameter with name 'foo' in '_ ptr: UnsafePointer<CInt>, _ len: CInt'}}
@PointerBounds(.countedBy(pointer: 1, count: "foo"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt) {
}
