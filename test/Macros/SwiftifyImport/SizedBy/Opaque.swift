// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -verify 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"))
func nonnullUnsafeRawBufferPointer(_ ptr: OpaquePointer, _ size: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"))
func nullableUnsafeRawBufferPointer(_ ptr: OpaquePointer?, _ size: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"))
func impNullableUnsafeRawBufferPointer(_ ptr: OpaquePointer!, _ size: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)))
func nonnullSpan(_ ptr: OpaquePointer, _ size: CInt) {
}

// expected-note@+2{{in expansion of macro '_SwiftifyImport' on global function 'nullableSpan' here}}
// Cannot refer to source location for the error: "type 'RawSpan' does not conform to protocol 'Escapable'" (which is currently necessary for Optional)
@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)))
func nullableSpan(_ ptr: OpaquePointer?, _ size: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)))
func impNullableSpan(_ ptr: OpaquePointer!, _ size: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func nonnullUnsafeRawBufferPointer(_ ptr: UnsafeRawBufferPointer) {
// CHECK-NEXT:     return nonnullUnsafeRawBufferPointer(OpaquePointer(ptr.baseAddress!), CInt(exactly: ptr.count)!)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func nullableUnsafeRawBufferPointer(_ ptr: UnsafeRawBufferPointer?) {
// CHECK-NEXT:     return nullableUnsafeRawBufferPointer(OpaquePointer(ptr?.baseAddress), CInt(exactly: ptr?.count ?? 0)!)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func impNullableUnsafeRawBufferPointer(_ ptr: UnsafeRawBufferPointer) {
// CHECK-NEXT:     return impNullableUnsafeRawBufferPointer(OpaquePointer(ptr.baseAddress!), CInt(exactly: ptr.count)!)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func nonnullSpan(_ ptr: RawSpan) {
// CHECK-NEXT:     return ptr.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:         return nonnullSpan(OpaquePointer(_ptrPtr.baseAddress!), CInt(exactly: ptr.byteCount)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func nullableSpan(_ ptr: RawSpan?) {
// CHECK-NEXT:     return if ptr == nil {
// CHECK-NEXT:         nullableSpan(nil, CInt(exactly: ptr?.byteCount ?? 0)!)
// CHECK-NEXT:     } else {
// CHECK-NEXT:         ptr!.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:             return nullableSpan(OpaquePointer(_ptrPtr.baseAddress), CInt(exactly: ptr?.byteCount ?? 0)!)
// CHECK-NEXT:         }
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func impNullableSpan(_ ptr: RawSpan) {
// CHECK-NEXT:     return ptr.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:         return impNullableSpan(OpaquePointer(_ptrPtr.baseAddress!), CInt(exactly: ptr.byteCount)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

