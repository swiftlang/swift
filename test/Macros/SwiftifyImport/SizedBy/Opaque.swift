// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions -verify 2>&1 | %FileCheck --match-full-lines %s

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

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)))
func nullableSpan(_ ptr: OpaquePointer?, _ size: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)))
func impNullableSpan(_ ptr: OpaquePointer!, _ size: CInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func nonnullUnsafeRawBufferPointer(_ ptr: UnsafeRawBufferPointer) {
// CHECK-NEXT:     return unsafe nonnullUnsafeRawBufferPointer(OpaquePointer(ptr.baseAddress!), CInt(exactly: ptr.count)!)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func nullableUnsafeRawBufferPointer(_ ptr: UnsafeRawBufferPointer?) {
// CHECK-NEXT:     return unsafe nullableUnsafeRawBufferPointer(OpaquePointer(ptr?.baseAddress), CInt(exactly: ptr?.count ?? 0)!)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func impNullableUnsafeRawBufferPointer(_ ptr: UnsafeRawBufferPointer) {
// CHECK-NEXT:     return unsafe impNullableUnsafeRawBufferPointer(OpaquePointer(ptr.baseAddress!), CInt(exactly: ptr.count)!)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func nonnullSpan(_ ptr: RawSpan) {
// CHECK-NEXT:     return unsafe ptr.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:         return unsafe nonnullSpan(OpaquePointer(_ptrPtr.baseAddress!), CInt(exactly: _ptrPtr.count)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func nullableSpan(_ ptr: RawSpan?) {
// CHECK-NEXT:     return { () in
// CHECK-NEXT:         return if ptr == nil {
// CHECK-NEXT:             unsafe nullableSpan(nil, CInt(exactly: ptr?.byteCount ?? 0)!)
// CHECK-NEXT:         } else {
// CHECK-NEXT:             unsafe ptr!.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:                 return unsafe nullableSpan(OpaquePointer(_ptrPtr.baseAddress), CInt(exactly: _ptrPtr.count)!)
// CHECK-NEXT:             }
// CHECK-NEXT:         }
// CHECK-NEXT:     }()
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func impNullableSpan(_ ptr: RawSpan) {
// CHECK-NEXT:     return unsafe ptr.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:         return unsafe impNullableSpan(OpaquePointer(_ptrPtr.baseAddress!), CInt(exactly: _ptrPtr.count)!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

