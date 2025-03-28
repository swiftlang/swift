// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions > %t.log 2>&1
// RUN: %FileCheck --match-full-lines %s < %t.log

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)))
func myFunc(_ ptr: UnsafeMutableRawPointer, _ size: CInt) {
}

// Emits UnsafeMutableRawBufferPointer until MutableRawSpan has landed

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ ptr: UnsafeMutableRawBufferPointer) {
// CHECK-NEXT:     return unsafe myFunc(ptr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT: }
