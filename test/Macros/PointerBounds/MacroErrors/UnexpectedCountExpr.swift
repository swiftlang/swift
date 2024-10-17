// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

import _PointerBounds

// expected-error@+1{{cannot convert value of type 'Int' to expected argument type 'String'}}
@PointerBounds(.countedBy(pointer: 1, count: 2))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: String) {
}
