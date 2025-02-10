// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .sizedBy(pointer: .param(2), size: "size"))
func myFunc(_ ptr: UnsafeRawPointer, _ ptr2: UnsafeRawPointer, _ size: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ ptr: UnsafeRawBufferPointer, _ ptr2: UnsafeRawBufferPointer, _ size: CInt) {
// CHECK-NEXT:     let _ptrCount: some BinaryInteger = size
// CHECK-NEXT:     if ptr.count < _ptrCount || _ptrCount < 0 {
// CHECK-NEXT:         fatalError("bounds check failure when calling unsafe function")
// CHECK-NEXT:     }
// CHECK-NEXT:     let _ptr2Count: some BinaryInteger = size
// CHECK-NEXT:     if ptr2.count < _ptr2Count || _ptr2Count < 0 {
// CHECK-NEXT:         fatalError("bounds check failure when calling unsafe function")
// CHECK-NEXT:     }
// CHECK-NEXT:     return myFunc(ptr.baseAddress!, ptr2.baseAddress!, size)
// CHECK-NEXT: }
