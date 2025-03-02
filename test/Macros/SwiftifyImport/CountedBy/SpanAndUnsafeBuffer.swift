// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

// rdar://145899513 ([StrictMemorySafety] Call to RawSpan::withUnsafeBytes not recognized as unsafe, while call to Span::withUnsafeBufferPointer is)
// RUN: not %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len1"), .countedBy(pointer: .param(3), count: "len2"), .nonescaping(pointer: .param(1)))
func myFunc(_ ptr1: UnsafePointer<CInt>, _ len1: CInt, _ ptr2: UnsafePointer<CInt>, _ len2: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ ptr1: Span<CInt>, _ ptr2: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     return unsafe ptr1.withUnsafeBufferPointer { _ptr1Ptr in
// CHECK-NEXT:         return unsafe myFunc(_ptr1Ptr.baseAddress!, CInt(exactly: ptr1.count)!, ptr2.baseAddress!, CInt(exactly: ptr2.count)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

