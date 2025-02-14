// REQUIRES: swift_swift_parser

// RUN: not %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions > %t.log 2>&1
// RUN: %FileCheck --match-full-lines %s < %t.log

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)))
func myFunc(_ ptr: UnsafeMutableRawPointer, _ size: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ ptr: MutableRawSpan) {
// CHECK-NEXT:     return ptr.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:         return myFunc(_ptrPtr.baseAddress!, CInt(exactly: ptr.byteCount)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }
