// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Lifetimes

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -enable-experimental-feature Lifetimes -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"))
func constParam(_ ptr: UnsafePointer<CChar>, _ size: CInt) {}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"))
func mutParam(_ ptr: UnsafeMutablePointer<UInt8>, _ size: CInt) {}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size * count"))
func exprParam(_ ptr: UnsafeMutablePointer<UInt8>, _ size: CInt, _ count: CInt) {}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "size"))
func constReturn(_ size: CInt) -> UnsafePointer<CChar> {}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "size"))
func mutReturn(_ size: CInt) -> UnsafeMutablePointer<UInt8> {}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "size * count"))
func exprReturn(_ size: CInt, _ count: CInt) -> UnsafeMutablePointer<UInt8> {}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"),
                 .nonescaping(pointer: .param(1)))
func constParamNoreturn(_ ptr: UnsafePointer<CChar>, _ size: CInt) {}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"),
                 .nonescaping(pointer: .param(1)))
func mutParamNoreturn(_ ptr: UnsafeMutablePointer<UInt8>, _ size: CInt) {}

@_SwiftifyImport(.sizedBy(pointer: .param(2), size: "size"),
                 .sizedBy(pointer: .return, size: "size"),
                 .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy))
func constReturnDependence(_ size: CInt, _ ptr: UnsafePointer<UInt8>) -> UnsafePointer<CChar> {}

@_SwiftifyImport(.sizedBy(pointer: .param(2), size: "size"),
                 .sizedBy(pointer: .return, size: "size"),
                 .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy))
func mutReturnDependence(_ size: CInt, _ ptr: UnsafeMutablePointer<UInt8>) -> UnsafeMutablePointer<UInt8> {}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func constParam(_ ptr: UnsafeRawBufferPointer) {
// CHECK-NEXT:     let size = CInt(exactly: unsafe ptr.count)!
// CHECK-NEXT:     return unsafe constParam(ptr.baseAddress!.assumingMemoryBound(to: CChar.self), size)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func mutParam(_ ptr: UnsafeMutableRawBufferPointer) {
// CHECK-NEXT:     let size = CInt(exactly: unsafe ptr.count)!
// CHECK-NEXT:     return unsafe mutParam(ptr.baseAddress!.assumingMemoryBound(to: UInt8.self), size)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func exprParam(_ ptr: UnsafeMutableRawBufferPointer, _ size: CInt, _ count: CInt) {
// CHECK-NEXT:     let _ptrCount = unsafe ptr.count
// CHECK-NEXT:     if _ptrCount != size * count {
// CHECK-NEXT:       fatalError("bounds check failure in exprParam: expected \(size * count) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe exprParam(ptr.baseAddress!.assumingMemoryBound(to: UInt8.self), size, count)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func constReturn(_ size: CInt) -> UnsafeRawBufferPointer {
// CHECK-NEXT:     return unsafe UnsafeRawBufferPointer(start: unsafe constReturn(size), count: Int(size))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func mutReturn(_ size: CInt) -> UnsafeMutableRawBufferPointer {
// CHECK-NEXT:     return unsafe UnsafeMutableRawBufferPointer(start: unsafe mutReturn(size), count: Int(size))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func exprReturn(_ size: CInt, _ count: CInt) -> UnsafeMutableRawBufferPointer {
// CHECK-NEXT:     return unsafe UnsafeMutableRawBufferPointer(start: unsafe exprReturn(size, count), count: Int(size * count))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func constParamNoreturn(_ ptr: RawSpan) {
// CHECK-NEXT:     let size = CInt(exactly: ptr.byteCount)!
// CHECK-NEXT:     return unsafe ptr.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:       return unsafe constParamNoreturn(_ptrPtr.baseAddress!.assumingMemoryBound(to: CChar.self), size)
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(ptr: copy ptr) @_disfavoredOverload
// CHECK-NEXT: func mutParamNoreturn(_ ptr: inout MutableRawSpan) {
// CHECK-NEXT:     let size = CInt(exactly: ptr.byteCount)!
// CHECK-NEXT:     return unsafe ptr.withUnsafeMutableBytes { _ptrPtr in
// CHECK-NEXT:       return unsafe mutParamNoreturn(_ptrPtr.baseAddress!.assumingMemoryBound(to: UInt8.self), size)
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(copy ptr) @_disfavoredOverload
// CHECK-NEXT: func constReturnDependence(_ ptr: RawSpan) -> RawSpan {
// CHECK-NEXT:     let size = CInt(exactly: ptr.byteCount)!
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe ptr.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:       return unsafe constReturnDependence(size, _ptrPtr.baseAddress!.assumingMemoryBound(to: UInt8.self))
// CHECK-NEXT:             }, byteCount: Int(size)), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(copy ptr) @_lifetime(ptr: copy ptr) @_disfavoredOverload
// CHECK-NEXT: func mutReturnDependence(_ ptr: inout MutableRawSpan) -> MutableRawSpan {
// CHECK-NEXT:     let size = CInt(exactly: ptr.byteCount)!
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: unsafe ptr.withUnsafeMutableBytes { _ptrPtr in
// CHECK-NEXT:       return unsafe mutReturnDependence(size, _ptrPtr.baseAddress!.assumingMemoryBound(to: UInt8.self))
// CHECK-NEXT:             }, byteCount: Int(size)), copying: ())
// CHECK-NEXT: }
