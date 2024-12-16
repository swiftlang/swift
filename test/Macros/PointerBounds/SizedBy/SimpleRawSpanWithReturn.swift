// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Span

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -enable-experimental-feature Span 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.sizedBy(pointer: 1, size: "size"), .nonescaping(pointer: 1))
func myFunc(_ ptr: UnsafeRawPointer, _ size: CInt) -> CInt {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ ptr: RawSpan) -> CInt {
// CHECK-NEXT:     return ptr.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:         return myFunc(_ptrPtr.baseAddress!, CInt(exactly: ptr.byteCount)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }
