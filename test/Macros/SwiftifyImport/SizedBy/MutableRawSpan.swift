// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions > %t.log 2>&1
// RUN: %FileCheck --match-full-lines %s < %t.log

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)))
func myFunc(_ ptr: UnsafeMutableRawPointer, _ size: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient @_lifetime(ptr: copy ptr) @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ ptr: inout MutableRawSpan) {
// CHECK-NEXT:     let size = CInt(exactly: ptr.byteCount)!
// CHECK-NEXT:     return unsafe ptr.withUnsafeMutableBytes { _ptrPtr in
// CHECK-NEXT:       return unsafe myFunc(_ptrPtr.baseAddress!, size)
// CHECK-NEXT:     }
// CHECK-NEXT: }
