// REQUIRES: swift_swift_parser
// REQUIRES: pointer_bounds

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck %s

import _PointerBounds

@PointerBounds(.countedBy(pointer: 1, count: "len"), .countedBy(pointer: 2, count: "len"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ ptr2: UnsafePointer<CInt>, _ len: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>, _ len: CInt) {
// CHECK-NEXT:     let _ptrCount: some BinaryInteger = len
// CHECK-NEXT:     if ptr.count < _ptrCount || _ptrCount < 0 {
// CHECK-NEXT:         fatalError("bounds check failure when calling unsafe function")
// CHECK-NEXT:     }
// CHECK-NEXT:     let _ptr2Count: some BinaryInteger = len
// CHECK-NEXT:     if ptr2.count < _ptr2Count || _ptr2Count < 0 {
// CHECK-NEXT:         fatalError("bounds check failure when calling unsafe function")
// CHECK-NEXT:     }
// CHECK-NEXT:     myFunc(ptr.baseAddress!, ptr2.baseAddress!, len)
// CHECK-NEXT: }
