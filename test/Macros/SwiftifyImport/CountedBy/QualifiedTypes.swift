// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func foo(_ ptr: Swift.UnsafePointer<Swift.Int>, _ len: Swift.Int) -> Swift.Void {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func bar(_ ptr: Swift.UnsafePointer<Swift.CInt>, _ len: Swift.Int) -> () {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func foo(_ ptr: Swift.UnsafeBufferPointer<Swift.Int>) -> Swift.Void {
// CHECK-NEXT:     let len = unsafe ptr.count
// CHECK-NEXT:     return unsafe foo(ptr.baseAddress!, len)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func bar(_ ptr: Swift.UnsafeBufferPointer<Swift.CInt>) -> () {
// CHECK-NEXT:     let len = unsafe ptr.count
// CHECK-NEXT:     return unsafe bar(ptr.baseAddress!, len)
// CHECK-NEXT: }
