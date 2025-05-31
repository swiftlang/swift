// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len1"), .countedBy(pointer: .param(3), count: "len2"), .nonescaping(pointer: .param(1)))
func myFunc(_ ptr1: UnsafePointer<CInt>, _ len1: CInt, _ ptr2: UnsafePointer<CInt>, _ len2: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ ptr1: Span<CInt>, _ ptr2: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     let len1 = CInt(exactly: ptr1.count)!
// CHECK-NEXT:     let len2 = CInt(exactly: unsafe ptr2.count)!
// CHECK-NEXT:     return unsafe ptr1.withUnsafeBufferPointer { _ptr1Ptr in
// CHECK-NEXT:       return unsafe myFunc(_ptr1Ptr.baseAddress!, len1, ptr2.baseAddress!, len2)
// CHECK-NEXT:     }
// CHECK-NEXT: }
