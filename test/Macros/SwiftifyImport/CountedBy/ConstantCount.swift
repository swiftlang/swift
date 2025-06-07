// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"))
func plain(_ ptr: UnsafePointer<CInt>) {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"))
func opt(_ ptr: UnsafePointer<CInt>?) {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"))
func mut(_ ptr: UnsafeMutablePointer<CInt>) {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"))
func mutOpt(_ ptr: UnsafeMutablePointer<CInt>?) {}


@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"), .nonescaping(pointer: .param(1)))
func noescape(_ ptr: UnsafePointer<CInt>) {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"), .nonescaping(pointer: .param(1)))
func noescapeOpt(_ ptr: UnsafePointer<CInt>?) {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"), .nonescaping(pointer: .param(1)))
func noescapeMut(_ ptr: UnsafeMutablePointer<CInt>) {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"), .nonescaping(pointer: .param(1)))
func noescapeMutOpt(_ ptr: UnsafeMutablePointer<CInt>?) {}


@_SwiftifyImport(.countedBy(pointer: .return, count: "37"))
func plainReturn() -> UnsafePointer<CInt> {}

@_SwiftifyImport(.countedBy(pointer: .return, count: "37"))
func optReturn() -> UnsafePointer<CInt>? {}

@_SwiftifyImport(.countedBy(pointer: .return, count: "37"))
func mutReturn() -> UnsafeMutablePointer<CInt> {}

@_SwiftifyImport(.countedBy(pointer: .return, count: "37"))
func mutOptReturn() -> UnsafeMutablePointer<CInt>? {}


@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
func noescape(_ ptr: UnsafePointer<CInt>) -> UnsafePointer<CInt> {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
func noescapeOpt(_ ptr: UnsafePointer<CInt>?) -> UnsafePointer<CInt>? {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
func noescapeMut(_ ptr: UnsafeMutablePointer<CInt>) -> UnsafeMutablePointer<CInt> {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
func noescapeMutOpt(_ ptr: UnsafeMutablePointer<CInt>?) -> UnsafeMutablePointer<CInt>? {}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func plain(_ ptr: UnsafeBufferPointer<CInt>) {
// CHECK-NEXT:     let _ptrCount = unsafe ptr.count
// CHECK-NEXT:     if _ptrCount != 37 {
// CHECK-NEXT:       fatalError("bounds check failure in plain: expected \(37) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe plain(ptr.baseAddress!)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func opt(_ ptr: UnsafeBufferPointer<CInt>?) {
// CHECK-NEXT:     let _ptrCount = unsafe ptr?.count ?? 0
// CHECK-NEXT:     if _ptrCount != 37 {
// CHECK-NEXT:       fatalError("bounds check failure in opt: expected \(37) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe opt(ptr?.baseAddress)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func mut(_ ptr: UnsafeMutableBufferPointer<CInt>) {
// CHECK-NEXT:     let _ptrCount = unsafe ptr.count
// CHECK-NEXT:     if _ptrCount != 37 {
// CHECK-NEXT:       fatalError("bounds check failure in mut: expected \(37) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe mut(ptr.baseAddress!)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func mutOpt(_ ptr: UnsafeMutableBufferPointer<CInt>?) {
// CHECK-NEXT:     let _ptrCount = unsafe ptr?.count ?? 0
// CHECK-NEXT:     if _ptrCount != 37 {
// CHECK-NEXT:       fatalError("bounds check failure in mutOpt: expected \(37) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe mutOpt(ptr?.baseAddress)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func noescape(_ ptr: Span<CInt>) {
// CHECK-NEXT:     let _ptrCount = ptr.count
// CHECK-NEXT:     if _ptrCount != 37 {
// CHECK-NEXT:       fatalError("bounds check failure in noescape: expected \(37) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe ptr.withUnsafeBufferPointer { _ptrPtr in
// CHECK-NEXT:       return unsafe noescape(_ptrPtr.baseAddress!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(copy ptr) @_disfavoredOverload
// CHECK-NEXT: func noescape(_ ptr: Span<CInt>) -> UnsafePointer<CInt> {
// CHECK-NEXT:     let _ptrCount = ptr.count
// CHECK-NEXT:     if _ptrCount != 37 {
// CHECK-NEXT:       fatalError("bounds check failure in noescape: expected \(37) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe ptr.withUnsafeBufferPointer { _ptrPtr in
// CHECK-NEXT:       return unsafe noescape(_ptrPtr.baseAddress!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func noescapeOpt(_ ptr: Span<CInt>?) {
// CHECK-NEXT:     let _ptrCount = ptr?.count ?? 0
// CHECK-NEXT:     if _ptrCount != 37 {
// CHECK-NEXT:       fatalError("bounds check failure in noescapeOpt: expected \(37) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return { () in
// CHECK-NEXT:         return if ptr == nil {
// CHECK-NEXT:             unsafe noescapeOpt(nil)
// CHECK-NEXT:           } else {
// CHECK-NEXT:             unsafe ptr!.withUnsafeBufferPointer { _ptrPtr in
// CHECK-NEXT:               return unsafe noescapeOpt(_ptrPtr.baseAddress)
// CHECK-NEXT:             }
// CHECK-NEXT:           }
// CHECK-NEXT:     }()
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(copy ptr) @_disfavoredOverload
// CHECK-NEXT: func noescapeOpt(_ ptr: Span<CInt>?) -> UnsafePointer<CInt>? {
// CHECK-NEXT:     let _ptrCount = ptr?.count ?? 0
// CHECK-NEXT:     if _ptrCount != 37 {
// CHECK-NEXT:       fatalError("bounds check failure in noescapeOpt: expected \(37) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return { () in
// CHECK-NEXT:         return if ptr == nil {
// CHECK-NEXT:             unsafe noescapeOpt(nil)
// CHECK-NEXT:           } else {
// CHECK-NEXT:             unsafe ptr!.withUnsafeBufferPointer { _ptrPtr in
// CHECK-NEXT:               return unsafe noescapeOpt(_ptrPtr.baseAddress)
// CHECK-NEXT:             }
// CHECK-NEXT:           }
// CHECK-NEXT:     }()
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(ptr: copy ptr) @_disfavoredOverload
// CHECK-NEXT: func noescapeMut(_ ptr: inout MutableSpan<CInt>) {
// CHECK-NEXT:     let _ptrCount = ptr.count
// CHECK-NEXT:     if _ptrCount != 37 {
// CHECK-NEXT:       fatalError("bounds check failure in noescapeMut: expected \(37) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe ptr.withUnsafeMutableBufferPointer { _ptrPtr in
// CHECK-NEXT:       return unsafe noescapeMut(_ptrPtr.baseAddress!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(copy ptr) @lifetime(ptr: copy ptr) @_disfavoredOverload
// CHECK-NEXT: func noescapeMut(_ ptr: inout MutableSpan<CInt>) -> UnsafeMutablePointer<CInt> {
// CHECK-NEXT:     let _ptrCount = ptr.count
// CHECK-NEXT:     if _ptrCount != 37 {
// CHECK-NEXT:       fatalError("bounds check failure in noescapeMut: expected \(37) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe ptr.withUnsafeMutableBufferPointer { _ptrPtr in
// CHECK-NEXT:       return unsafe noescapeMut(_ptrPtr.baseAddress!)
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(ptr: copy ptr) @_disfavoredOverload
// CHECK-NEXT: func noescapeMutOpt(_ ptr: inout MutableSpan<CInt>?) {
// CHECK-NEXT:     let _ptrCount = ptr?.count ?? 0
// CHECK-NEXT:     if _ptrCount != 37 {
// CHECK-NEXT:       fatalError("bounds check failure in noescapeMutOpt: expected \(37) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return { () in
// CHECK-NEXT:         return if ptr == nil {
// CHECK-NEXT:             unsafe noescapeMutOpt(nil)
// CHECK-NEXT:           } else {
// CHECK-NEXT:             unsafe ptr!.withUnsafeMutableBufferPointer { _ptrPtr in
// CHECK-NEXT:               return unsafe noescapeMutOpt(_ptrPtr.baseAddress)
// CHECK-NEXT:             }
// CHECK-NEXT:           }
// CHECK-NEXT:     }()
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(copy ptr) @lifetime(ptr: copy ptr) @_disfavoredOverload
// CHECK-NEXT: func noescapeMutOpt(_ ptr: inout MutableSpan<CInt>?) -> UnsafeMutablePointer<CInt>? {
// CHECK-NEXT:     let _ptrCount = ptr?.count ?? 0
// CHECK-NEXT:     if _ptrCount != 37 {
// CHECK-NEXT:       fatalError("bounds check failure in noescapeMutOpt: expected \(37) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return { () in
// CHECK-NEXT:         return if ptr == nil {
// CHECK-NEXT:             unsafe noescapeMutOpt(nil)
// CHECK-NEXT:           } else {
// CHECK-NEXT:             unsafe ptr!.withUnsafeMutableBufferPointer { _ptrPtr in
// CHECK-NEXT:               return unsafe noescapeMutOpt(_ptrPtr.baseAddress)
// CHECK-NEXT:             }
// CHECK-NEXT:           }
// CHECK-NEXT:     }()
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func plainReturn() -> UnsafeBufferPointer<CInt> {
// CHECK-NEXT:     return unsafe UnsafeBufferPointer<CInt> (start: unsafe plainReturn(), count: Int(37))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func optReturn() -> UnsafeBufferPointer<CInt>? {
// CHECK-NEXT:     return unsafe { () in
// CHECK-NEXT:       let _resultValue = unsafe optReturn()
// CHECK-NEXT:       if unsafe _resultValue == nil {
// CHECK-NEXT:         return nil
// CHECK-NEXT:       } else {
// CHECK-NEXT:         return unsafe _swiftifyOverrideLifetime(UnsafeBufferPointer<CInt>(start: _resultValue!, count: Int(37)), copying: ())
// CHECK-NEXT:       }
// CHECK-NEXT:     }()
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func mutReturn() -> UnsafeMutableBufferPointer<CInt> {
// CHECK-NEXT:     return unsafe UnsafeMutableBufferPointer<CInt> (start: unsafe mutReturn(), count: Int(37))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func mutOptReturn() -> UnsafeMutableBufferPointer<CInt>? {
// CHECK-NEXT:     return unsafe { () in
// CHECK-NEXT:       let _resultValue = unsafe mutOptReturn()
// CHECK-NEXT:       if unsafe _resultValue == nil {
// CHECK-NEXT:         return nil
// CHECK-NEXT:       } else {
// CHECK-NEXT:         return unsafe _swiftifyOverrideLifetime(UnsafeMutableBufferPointer<CInt>(start: _resultValue!, count: Int(37)), copying: ())
// CHECK-NEXT:       }
// CHECK-NEXT:     }()
// CHECK-NEXT: }
