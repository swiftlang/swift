// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -enable-experimental-feature SymbolLinkageMarkers -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck %s

import _PointerBounds

@PointerBounds(.sizedBy(pointer: 1, size: "size"), .nonescaping(pointer: 1))
func myFunc(_ ptr: UnsafeMutableRawPointer, _ size: CInt) {
}

// CHECK:      func myFunc(_ ptr: MutableRawSpan) {
// CHECK-NEXT:     ptr.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:         myFunc(_ptrPtr.baseAddress!, CInt(exactly: ptr.byteCount)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }
