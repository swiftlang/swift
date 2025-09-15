// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -strict-memory-safety -warnings-as-errors 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .sizedBy(pointer: .param(2), size: "size"))
func myFunc(_ ptr: UnsafeRawPointer, _ ptr2: UnsafeRawPointer, _ size: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ ptr: UnsafeRawBufferPointer, _ ptr2: UnsafeRawBufferPointer) {
// CHECK-NEXT:     let size = CInt(exactly: unsafe ptr.count)!
// CHECK-NEXT:     if unsafe ptr2.count != size {
// CHECK-NEXT:       fatalError("bounds check failure in myFunc: expected \(size) but got \(unsafe ptr2.count)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe myFunc(ptr.baseAddress!, ptr2.baseAddress!, size)
// CHECK-NEXT: }
