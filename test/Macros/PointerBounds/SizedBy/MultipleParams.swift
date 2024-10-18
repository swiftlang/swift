// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck %s

import _PointerBounds

@PointerBounds(.sizedBy(pointer: 1, size: "size"), .sizedBy(pointer: 3, size: "size2"))
func myFunc(_ ptr: UnsafeRawPointer, _ size: CInt, _ ptr2: UnsafeRawPointer, _ size2: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ ptr: UnsafeRawBufferPointer, _ ptr2: UnsafeRawBufferPointer) {
// CHECK-NEXT:     myFunc(ptr.baseAddress!, CInt(exactly: ptr.count)!, ptr2.baseAddress!, CInt(exactly: ptr2.count)!)
// CHECK-NEXT: }
