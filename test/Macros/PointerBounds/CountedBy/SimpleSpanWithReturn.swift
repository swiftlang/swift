// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -enable-experimental-feature SymbolLinkageMarkers -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck %s

import _PointerBounds

@PointerBounds(.countedBy(pointer: 1, count: "len"), .nonescaping(pointer: 1))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt) -> CInt {
}

// CHECK:      func myFunc(_ ptr: Span<CInt>) -> CInt {
// CHECK-NEXT:     return ptr.withUnsafeBufferPointer { _ptrPtr in
// CHECK-NEXT:         return myFunc(_ptrPtr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }
