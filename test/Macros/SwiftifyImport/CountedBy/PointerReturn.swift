// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Lifetimes

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -enable-experimental-feature Lifetimes -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: .return, count: "len"))
func myFunc(_ len: CInt) -> UnsafeMutablePointer<CInt> {
}

@_SwiftifyImport(.countedBy(pointer: .return, count: "len"), .nonescaping(pointer: .return))
func nonEscaping(_ len: CInt) -> UnsafePointer<CInt> {
}

@_SwiftifyImport(.countedBy(pointer: .return, count: "len2"), .countedBy(pointer: .param(1), count: "len1"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
func lifetimeDependentCopy(_ p: UnsafePointer<CInt>, _ len1: CInt, _ len2: CInt) -> UnsafePointer<CInt> {
}

@_SwiftifyImport(.countedBy(pointer: .return, count: "len2"), .countedBy(pointer: .param(1), count: "len1"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow))
func lifetimeDependentBorrow(_ p: borrowing UnsafePointer<CInt>, _ len1: CInt, _ len2: CInt) -> UnsafePointer<CInt> {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {
// CHECK-NEXT:     return unsafe UnsafeMutableBufferPointer<CInt> (start: unsafe myFunc(len), count: Int(len))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func nonEscaping(_ len: CInt) -> UnsafeBufferPointer<CInt> {
// CHECK-NEXT:     return unsafe UnsafeBufferPointer<CInt> (start: unsafe nonEscaping(len), count: Int(len))

// CHECK:      @_alwaysEmitIntoClient @_lifetime(copy p) @_disfavoredOverload
// CHECK-NEXT: func lifetimeDependentCopy(_ p: Span<CInt>, _ len2: CInt) -> Span<CInt> {
// CHECK-NEXT:     let len1 = CInt(exactly: p.count)!
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(Span<CInt> (_unsafeStart: unsafe p.withUnsafeBufferPointer { _pPtr in
// CHECK-NEXT:       return unsafe lifetimeDependentCopy(_pPtr.baseAddress!, len1, len2)
// CHECK-NEXT:             }, count: Int(len2)), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(borrow p) @_disfavoredOverload
// CHECK-NEXT: func lifetimeDependentBorrow(_ p: borrowing UnsafeBufferPointer<CInt>, _ len2: CInt) -> Span<CInt> {
// CHECK-NEXT:     let len1 = CInt(exactly: unsafe p.count)!
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(Span<CInt> (_unsafeStart: unsafe lifetimeDependentBorrow(p.baseAddress!, len1, len2), count: Int(len2)), copying: ())
// CHECK-NEXT: }
