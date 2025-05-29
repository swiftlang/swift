// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions > %t.log 2>&1
// RUN: %FileCheck --match-full-lines %s < %t.log

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)))
func myFunc(_ ptr: UnsafeMutablePointer<CInt>, _ len: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient @lifetime(ptr: copy ptr) @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ ptr: inout MutableSpan<CInt>) {
// CHECK-NEXT:     let len = CInt(exactly: ptr.count)!
// CHECK-NEXT:     return unsafe ptr.withUnsafeMutableBufferPointer { _ptrPtr in
// CHECK-NEXT:       return unsafe myFunc(_ptrPtr.baseAddress!, len)
// CHECK-NEXT:     }
// CHECK-NEXT: }
