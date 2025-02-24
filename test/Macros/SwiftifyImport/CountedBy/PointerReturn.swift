// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_LifetimeDependence

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -enable-experimental-feature LifetimeDependence -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

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
// CHECK-NEXT:     return UnsafeMutableBufferPointer<CInt> (start: myFunc(len), count: Int(len))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func nonEscaping(_ len: CInt) -> UnsafeBufferPointer<CInt> {
// CHECK-NEXT:     return UnsafeBufferPointer<CInt> (start: nonEscaping(len), count: Int(len))

// CHECK:      @_alwaysEmitIntoClient @lifetime(p)
// CHECK-NEXT: func lifetimeDependentCopy(_ p: Span<CInt>, _ len2: CInt) -> Span<CInt> {
// CHECK-NEXT:     return Span<CInt> (_unsafeStart:   p.withUnsafeBufferPointer { _pPtr in
// CHECK-NEXT:         return lifetimeDependentCopy(_pPtr.baseAddress!, CInt(exactly: p.count)!, len2)
// CHECK-NEXT:       }, count: Int(len2))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(borrow p)
// CHECK-NEXT: func lifetimeDependentBorrow(_ p: borrowing UnsafeBufferPointer<CInt>, _ len2: CInt) -> Span<CInt> {
// CHECK-NEXT:     return Span<CInt> (_unsafeStart: lifetimeDependentBorrow(p.baseAddress!, CInt(exactly: p.count)!, len2), count: Int(len2))
// CHECK-NEXT: }

