// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -enable-experimental-feature SymbolLinkageMarkers -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck %s

import _PointerBounds

@PointerBounds(.sizedBy(pointer: 1, size: "size"))
func myFunc(_ ptr: UnsafeRawPointer, _ size: CInt) {
}

// CHECK:      func myFunc(_ ptr: UnsafeRawBufferPointer) {
// CHECK-NEXT:     myFunc(ptr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT: }
