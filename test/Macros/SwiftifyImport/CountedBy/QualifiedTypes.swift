// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func foo(_ ptr: Swift.UnsafePointer<Swift.Int>, _ len: Swift.Int) -> Swift.Void {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func bar(_ ptr: Swift.UnsafePointer<Swift.CInt>, _ len: Swift.Int) -> () {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func foo(_ ptr: Swift.UnsafeBufferPointer<Swift.Int>) -> Swift.Void {
// CHECK-NEXT:     return foo(ptr.baseAddress!, ptr.count)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func bar(_ ptr: Swift.UnsafeBufferPointer<Swift.CInt>) -> () {
// CHECK-NEXT:     return bar(ptr.baseAddress!, ptr.count)
// CHECK-NEXT: }


