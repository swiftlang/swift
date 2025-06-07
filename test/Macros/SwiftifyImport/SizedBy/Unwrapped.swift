// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "len"))
func myFunc(_ ptr: UnsafeRawPointer!, _ len: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ ptr: UnsafeRawBufferPointer) {
// CHECK-NEXT:     let len = CInt(exactly: unsafe ptr.count)!
// CHECK-NEXT:     return unsafe myFunc(ptr.baseAddress!, len)
// CHECK-NEXT: }
