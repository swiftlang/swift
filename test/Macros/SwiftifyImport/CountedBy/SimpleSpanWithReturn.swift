// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s
// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt) -> CInt {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ ptr: Span<CInt>) -> CInt {
// CHECK-NEXT:     return unsafe ptr.withUnsafeBufferPointer { _ptrPtr in
// CHECK-NEXT:         return unsafe myFunc(_ptrPtr.baseAddress!, CInt(exactly: _ptrPtr.count)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }
