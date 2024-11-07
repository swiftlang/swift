// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -enable-experimental-feature SymbolLinkageMarkers -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck %s

import _PointerBounds

@PointerBounds(.countedBy(pointer: 1, count: "len"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt) -> CInt {
}

// CHECK:      func myFunc(_ ptr: UnsafeBufferPointer<CInt>) -> CInt {
// CHECK-NEXT:     return myFunc(ptr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT: }
