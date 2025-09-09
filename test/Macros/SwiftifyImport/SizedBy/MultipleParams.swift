// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -strict-memory-safety -warnings-as-errors 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .sizedBy(pointer: .param(3), size: "size2"))
func myFunc(_ ptr: UnsafeRawPointer, _ size: CInt, _ ptr2: UnsafeRawPointer, _ size2: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ ptr: UnsafeRawBufferPointer, _ ptr2: UnsafeRawBufferPointer) {
// CHECK-NEXT:     let size = CInt(exactly: unsafe ptr.count)!
// CHECK-NEXT:     let size2 = CInt(exactly: unsafe ptr2.count)!
// CHECK-NEXT:     return unsafe myFunc(ptr.baseAddress!, size, ptr2.baseAddress!, size2)
// CHECK-NEXT: }
