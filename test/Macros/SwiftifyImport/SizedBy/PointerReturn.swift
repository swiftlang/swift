// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_LifetimeDependence

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -enable-experimental-feature LifetimeDependence -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.sizedBy(pointer: .return, size: "len"))
func myFunc(_ len: CInt) -> UnsafeMutableRawPointer {
}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "len"), .nonescaping(pointer: .return))
func nonEscaping(_ len: CInt) -> UnsafeRawPointer {
}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "len2"), .sizedBy(pointer: .param(1), size: "len1"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
func lifetimeDependentCopy(_ p: UnsafeRawPointer, _ len1: CInt, _ len2: CInt) -> UnsafeRawPointer {
}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "len2"), .sizedBy(pointer: .param(1), size: "len1"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow))
func lifetimeDependentBorrow(_ p: borrowing UnsafeRawPointer, _ len1: CInt, _ len2: CInt) -> UnsafeRawPointer {
}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "len2"), .sizedBy(pointer: .param(1), size: "len1"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
func lifetimeDependentCopyMut(_ p: UnsafeMutableRawPointer, _ len1: CInt, _ len2: CInt) -> UnsafeMutableRawPointer {
}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "len2"), .sizedBy(pointer: .param(1), size: "len1"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow))
func lifetimeDependentBorrowMut(_ p: borrowing UnsafeMutableRawPointer, _ len1: CInt, _ len2: CInt) -> UnsafeMutableRawPointer {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ len: CInt) -> UnsafeMutableRawBufferPointer {
// CHECK-NEXT:     return unsafe UnsafeMutableRawBufferPointer(start: unsafe myFunc(len), count: Int(len))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func nonEscaping(_ len: CInt) -> UnsafeRawBufferPointer {
// CHECK-NEXT:     return unsafe UnsafeRawBufferPointer(start: unsafe nonEscaping(len), count: Int(len))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(copy p) @_disfavoredOverload
// CHECK-NEXT: func lifetimeDependentCopy(_ p: RawSpan, _ len2: CInt) -> RawSpan {
// CHECK-NEXT:     let len1 = CInt(exactly: p.byteCount)!
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe p.withUnsafeBytes { _pPtr in
// CHECK-NEXT:       return unsafe lifetimeDependentCopy(_pPtr.baseAddress!, len1, len2)
// CHECK-NEXT:             }, byteCount: Int(len2)), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(borrow p) @_disfavoredOverload
// CHECK-NEXT: func lifetimeDependentBorrow(_ p: borrowing UnsafeRawBufferPointer, _ len2: CInt) -> RawSpan {
// CHECK-NEXT:     let len1 = CInt(exactly: unsafe p.count)!
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe lifetimeDependentBorrow(p.baseAddress!, len1, len2), byteCount: Int(len2)), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(copy p) @lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT: func lifetimeDependentCopyMut(_ p: inout MutableRawSpan, _ len2: CInt) -> MutableRawSpan {
// CHECK-NEXT:     let len1 = CInt(exactly: p.byteCount)!
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: unsafe p.withUnsafeMutableBytes { _pPtr in
// CHECK-NEXT:       return unsafe lifetimeDependentCopyMut(_pPtr.baseAddress!, len1, len2)
// CHECK-NEXT:             }, byteCount: Int(len2)), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(borrow p) @_disfavoredOverload
// CHECK-NEXT: func lifetimeDependentBorrowMut(_ p: borrowing UnsafeMutableRawBufferPointer, _ len2: CInt) -> MutableRawSpan {
// CHECK-NEXT:     let len1 = CInt(exactly: unsafe p.count)!
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: unsafe lifetimeDependentBorrowMut(p.baseAddress!, len1, len2), byteCount: Int(len2)), copying: ())
// CHECK-NEXT: }
