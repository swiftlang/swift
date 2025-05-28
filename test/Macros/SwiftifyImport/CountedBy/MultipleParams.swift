// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .countedBy(pointer: .param(3), count: "len2"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt, _ ptr2: UnsafePointer<CInt>, _ len2: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     let len = CInt(exactly: unsafe ptr.count)!
// CHECK-NEXT:     let len2 = CInt(exactly: unsafe ptr2.count)!
// CHECK-NEXT:     return unsafe myFunc(ptr.baseAddress!, len, ptr2.baseAddress!, len2)
// CHECK-NEXT: }
