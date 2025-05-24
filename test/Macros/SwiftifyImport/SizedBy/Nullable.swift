// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"))
func myFunc(_ ptr: UnsafeRawPointer?, _ size: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "len"), .nonescaping(pointer: .param(1)))
func myFunc2(_ ptr: UnsafeMutableRawPointer?, _ len: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "len"), .nonescaping(pointer: .param(1)), .sizedBy(pointer: .param(3), size: "len2"), .nonescaping(pointer: .param(3)))
func myFunc3(_ ptr: UnsafeMutableRawPointer?, _ len: CInt, _ ptr2: UnsafeMutableRawPointer?, _ len2: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "len"), .sizedBy(pointer: .return, size: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
func myFunc4(_ ptr: UnsafeMutableRawPointer?, _ len: CInt) -> UnsafeMutableRawPointer? {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ ptr: UnsafeRawBufferPointer?) {
// CHECK-NEXT:     let size = CInt(exactly: unsafe ptr?.count ?? 0)!
// CHECK-NEXT:     return unsafe myFunc(ptr?.baseAddress, size)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(ptr: copy ptr) @_disfavoredOverload
// CHECK-NEXT: func myFunc2(_ ptr: inout MutableRawSpan?) {
// CHECK-NEXT:     let len = CInt(exactly: ptr?.byteCount ?? 0)!
// CHECK-NEXT:     return { () in
// CHECK-NEXT:         return if ptr == nil {
// CHECK-NEXT:             unsafe myFunc2(nil, len)
// CHECK-NEXT:           } else {
// CHECK-NEXT:             unsafe ptr!.withUnsafeMutableBytes { _ptrPtr in
// CHECK-NEXT:               return unsafe myFunc2(_ptrPtr.baseAddress, len)
// CHECK-NEXT:             }
// CHECK-NEXT:           }
// CHECK-NEXT:     }()
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(ptr: copy ptr) @lifetime(ptr2: copy ptr2) @_disfavoredOverload
// CHECK-NEXT: func myFunc3(_ ptr: inout MutableRawSpan?, _ ptr2: inout MutableRawSpan?) {
// CHECK-NEXT:     let len = CInt(exactly: ptr?.byteCount ?? 0)!
// CHECK-NEXT:     let len2 = CInt(exactly: ptr2?.byteCount ?? 0)!
// CHECK-NEXT:     return { () in
// CHECK-NEXT:         return if ptr2 == nil {
// CHECK-NEXT:             { () in
// CHECK-NEXT:                 return if ptr == nil {
// CHECK-NEXT:                         unsafe myFunc3(nil, len, nil, len2)
// CHECK-NEXT:                       } else {
// CHECK-NEXT:                         unsafe ptr!.withUnsafeMutableBytes { _ptrPtr in
// CHECK-NEXT:                           return unsafe myFunc3(_ptrPtr.baseAddress, len, nil, len2)
// CHECK-NEXT:                         }
// CHECK-NEXT:                       }
// CHECK-NEXT:             }()
// CHECK-NEXT:           } else {
// CHECK-NEXT:             unsafe ptr2!.withUnsafeMutableBytes { _ptr2Ptr in
// CHECK-NEXT:               return { () in
// CHECK-NEXT:                   return if ptr == nil {
// CHECK-NEXT:                           unsafe myFunc3(nil, len, _ptr2Ptr.baseAddress, len2)
// CHECK-NEXT:                         } else {
// CHECK-NEXT:                           unsafe ptr!.withUnsafeMutableBytes { _ptrPtr in
// CHECK-NEXT:                             return unsafe myFunc3(_ptrPtr.baseAddress, len, _ptr2Ptr.baseAddress, len2)
// CHECK-NEXT:                           }
// CHECK-NEXT:                         }
// CHECK-NEXT:               }()
// CHECK-NEXT:             }
// CHECK-NEXT:           }
// CHECK-NEXT:     }()
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(copy ptr) @lifetime(ptr: copy ptr) @_disfavoredOverload
// CHECK-NEXT: func myFunc4(_ ptr: inout MutableRawSpan?) -> MutableRawSpan? {
// CHECK-NEXT:     let len = CInt(exactly: ptr?.byteCount ?? 0)!
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime({ () in
// CHECK-NEXT:       let _resultValue = { () in
// CHECK-NEXT:               return if ptr == nil {
// CHECK-NEXT:                   unsafe myFunc4(nil, len)
// CHECK-NEXT:                 } else {
// CHECK-NEXT:                   unsafe ptr!.withUnsafeMutableBytes { _ptrPtr in
// CHECK-NEXT:                     return unsafe myFunc4(_ptrPtr.baseAddress, len)
// CHECK-NEXT:                   }
// CHECK-NEXT:                 }
// CHECK-NEXT:           }()
// CHECK-NEXT:       if unsafe _resultValue == nil {
// CHECK-NEXT:         return nil
// CHECK-NEXT:       } else {
// CHECK-NEXT:         return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: _resultValue!, byteCount: Int(len)), copying: ())
// CHECK-NEXT:       }
// CHECK-NEXT:         }(), copying: ())
// CHECK-NEXT: }
