// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
func myFunc(_ ptr: UnsafePointer<CInt>?, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)))
func myFunc2(_ ptr: UnsafeMutablePointer<CInt>?, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)), .countedBy(pointer: .param(3), count: "len2"), .nonescaping(pointer: .param(3)))
func myFunc3(_ ptr: UnsafeMutablePointer<CInt>?, _ len: CInt, _ ptr2: UnsafeMutablePointer<CInt>?, _ len2: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .countedBy(pointer: .return, count: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
func myFunc4(_ ptr: UnsafeMutablePointer<CInt>?, _ len: CInt) -> UnsafeMutablePointer<CInt>? {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ ptr: UnsafeBufferPointer<CInt>?) {
// CHECK-NEXT:     let len = CInt(exactly: unsafe ptr?.count ?? 0)!
// CHECK-NEXT:     return unsafe myFunc(ptr?.baseAddress, len)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(ptr: copy ptr) @_disfavoredOverload
// CHECK-NEXT: func myFunc2(_ ptr: inout MutableSpan<CInt>?) {
// CHECK-NEXT:     let len = CInt(exactly: ptr?.count ?? 0)!
// CHECK-NEXT:     return { () in
// CHECK-NEXT:         return if ptr == nil {
// CHECK-NEXT:             unsafe myFunc2(nil, len)
// CHECK-NEXT:         } else {
// CHECK-NEXT:             unsafe ptr!.withUnsafeMutableBufferPointer { _ptrPtr in
// CHECK-NEXT:                 return unsafe myFunc2(_ptrPtr.baseAddress, len)
// CHECK-NEXT:             }
// CHECK-NEXT:         }
// CHECK-NEXT:     }()
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(ptr: copy ptr) @lifetime(ptr2: copy ptr2) @_disfavoredOverload
// CHECK-NEXT: func myFunc3(_ ptr: inout MutableSpan<CInt>?, _ ptr2: inout MutableSpan<CInt>?) {
// CHECK-NEXT:     let len = CInt(exactly: ptr?.count ?? 0)!
// CHECK-NEXT:     let len2 = CInt(exactly: ptr2?.count ?? 0)!
// CHECK-NEXT:     return { () in
// CHECK-NEXT:         return if ptr2 == nil {
// CHECK-NEXT:             { () in
// CHECK-NEXT:                 return  if ptr == nil {
// CHECK-NEXT:                     unsafe myFunc3(nil, len, nil, len2)
// CHECK-NEXT:                 } else {
// CHECK-NEXT:                     unsafe ptr!.withUnsafeMutableBufferPointer { _ptrPtr in
// CHECK-NEXT:                         return unsafe myFunc3(_ptrPtr.baseAddress, len, nil, len2)
// CHECK-NEXT:                     }
// CHECK-NEXT:                 }
// CHECK-NEXT:             }()
// CHECK-NEXT:         } else {
// CHECK-NEXT:             unsafe ptr2!.withUnsafeMutableBufferPointer { _ptr2Ptr in
// CHECK-NEXT:                 return { () in
// CHECK-NEXT:                     return if ptr == nil {
// CHECK-NEXT:                         unsafe myFunc3(nil, len, _ptr2Ptr.baseAddress, len2)
// CHECK-NEXT:                     } else {
// CHECK-NEXT:                         unsafe ptr!.withUnsafeMutableBufferPointer { _ptrPtr in
// CHECK-NEXT:                             return unsafe myFunc3(_ptrPtr.baseAddress, len, _ptr2Ptr.baseAddress, len2)
// CHECK-NEXT:                         }
// CHECK-NEXT:                     }
// CHECK-NEXT:                 }()
// CHECK-NEXT:             }
// CHECK-NEXT:         }
// CHECK-NEXT:     }()
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(copy ptr) @lifetime(ptr: copy ptr) @_disfavoredOverload
// CHECK-NEXT: func myFunc4(_ ptr: inout MutableSpan<CInt>?) -> MutableSpan<CInt>? {
// CHECK-NEXT:     let len = CInt(exactly: ptr?.count ?? 0)!
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime({ () in
// CHECK-NEXT:         let _resultValue = { () in
// CHECK-NEXT:             return if ptr == nil {
// CHECK-NEXT:                 unsafe myFunc4(nil, len)
// CHECK-NEXT:             } else {
// CHECK-NEXT:                 unsafe ptr!.withUnsafeMutableBufferPointer { _ptrPtr in
// CHECK-NEXT:                     return unsafe myFunc4(_ptrPtr.baseAddress, len)
// CHECK-NEXT:                 }
// CHECK-NEXT:             }
// CHECK-NEXT:         }()
// CHECK-NEXT:         if unsafe _resultValue == nil {
// CHECK-NEXT:             return nil
// CHECK-NEXT:         } else {
// CHECK-NEXT:             return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())
// CHECK-NEXT:         }
// CHECK-NEXT:     }(), copying: ())
// CHECK-NEXT: }
