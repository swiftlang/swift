// REQUIRES: swift_swift_parser

// RUN: not %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions > %t.log 2>&1
// RUN: %FileCheck --match-full-lines %s < %t.log

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)))
func myFunc(_ ptr: UnsafeMutablePointer<CInt>, _ len: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ ptr: MutableSpan<CInt>) {
// CHECK-NEXT:     return unsafe ptr.withUnsafeBufferPointer { _ptrPtr in
// CHECK-NEXT:         return unsafe myFunc(_ptrPtr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

