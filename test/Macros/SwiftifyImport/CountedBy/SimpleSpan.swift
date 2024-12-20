// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Span

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -enable-experimental-feature Span 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: 1, count: "len"), .nonescaping(pointer: 1))
func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ ptr: Span<CInt>) {
// CHECK-NEXT:     return ptr.withUnsafeBufferPointer { _ptrPtr in
// CHECK-NEXT:         return myFunc(_ptrPtr.baseAddress!, CInt(exactly: ptr.count)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }
