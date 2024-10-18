// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

import _PointerBounds

@PointerBounds(.countedBy(pointer: 1, count: "len"))
// expected-error@+1{{expected pointer type, got [CInt] with kind arrayType}}
func myFunc(_ ptr: [CInt], _ len: String) {
}
