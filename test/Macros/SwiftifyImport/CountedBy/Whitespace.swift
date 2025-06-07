@_SwiftifyImport(.countedBy(pointer: .return, count: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), .countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)), .countedBy(pointer: .param(3), count: "len2"), .nonescaping(pointer: .param(3)))
func myFunc(_ ptr: UnsafeMutablePointer<CInt>?, _ len: CInt, _ ptr2: UnsafeMutablePointer<CInt>?, _ len2: CInt) -> UnsafeMutablePointer<CInt>? {}

// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines --strict-whitespace %s

// This test is meant to act as an alarm bell to unintended changes in whitespace

// CHECK:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @_lifetime(copy ptr) @_lifetime(ptr: copy ptr) @_lifetime(ptr2: copy ptr2) @_disfavoredOverload
// CHECK-NEXT:func myFunc(_ ptr: inout MutableSpan<CInt>?, _ ptr2: inout MutableSpan<CInt>?) -> MutableSpan<CInt>? {
// CHECK-NEXT:    let len = CInt(exactly: ptr?.count ?? 0)!
// CHECK-NEXT:    let len2 = CInt(exactly: ptr2?.count ?? 0)!
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime({ () in
// CHECK-NEXT:      let _resultValue = { () in
// CHECK-NEXT:              return if ptr2 == nil {
// CHECK-NEXT:                  { () in
// CHECK-NEXT:                      return if ptr == nil {
// CHECK-NEXT:                              unsafe myFunc(nil, len, nil, len2)
// CHECK-NEXT:                            } else {
// CHECK-NEXT:                              unsafe ptr!.withUnsafeMutableBufferPointer { _ptrPtr in
// CHECK-NEXT:                                return unsafe myFunc(_ptrPtr.baseAddress, len, nil, len2)
// CHECK-NEXT:                              }
// CHECK-NEXT:                            }
// CHECK-NEXT:                  }()
// CHECK-NEXT:                } else {
// CHECK-NEXT:                  unsafe ptr2!.withUnsafeMutableBufferPointer { _ptr2Ptr in
// CHECK-NEXT:                    return { () in
// CHECK-NEXT:                        return if ptr == nil {
// CHECK-NEXT:                                unsafe myFunc(nil, len, _ptr2Ptr.baseAddress, len2)
// CHECK-NEXT:                              } else {
// CHECK-NEXT:                                unsafe ptr!.withUnsafeMutableBufferPointer { _ptrPtr in
// CHECK-NEXT:                                  return unsafe myFunc(_ptrPtr.baseAddress, len, _ptr2Ptr.baseAddress, len2)
// CHECK-NEXT:                                }
// CHECK-NEXT:                              }
// CHECK-NEXT:                    }()
// CHECK-NEXT:                  }
// CHECK-NEXT:                }
// CHECK-NEXT:          }()
// CHECK-NEXT:      if unsafe _resultValue == nil {
// CHECK-NEXT:        return nil
// CHECK-NEXT:      } else {
// CHECK-NEXT:        return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())
// CHECK-NEXT:      }
// CHECK-NEXT:        }(), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
