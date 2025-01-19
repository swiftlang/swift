// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .countedBy(pointer: .param(3), count: "len2"))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt, _ ptr2: UnsafePointer<CInt>, _ len2: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     return myFunc(ptr.baseAddress!, CInt(exactly: ptr.count)!, ptr2.baseAddress!, CInt(exactly: ptr2.count)!)
// CHECK-NEXT: }
