// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// XFAIL: *

// RUN: not %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck %s
// RUN: %target-typecheck-verify-swift %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -verify

import _PointerBounds

@PointerBounds(.countedBy(pointer: 1, count: "len"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: String) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     myFunc(ptr.baseAddress!, String(exactly: ptr.count)!)
// CHECK-NEXT: }

// expected-error@PointerBounds:2{{no exact matches in call to initializer}}
