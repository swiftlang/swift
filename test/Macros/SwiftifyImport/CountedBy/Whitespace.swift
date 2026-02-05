// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Lifetimes
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -enable-experimental-feature Lifetimes -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -enable-experimental-feature Lifetimes 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

// This test is meant to act as an alarm bell to unintended changes in whitespace

//--- test.swift
@_SwiftifyImport(.countedBy(pointer: .return, count: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), .countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)), .countedBy(pointer: .param(3), count: "len2"), .nonescaping(pointer: .param(3)))
public func myFunc(_ ptr: UnsafeMutablePointer<CInt>?, _ len: CInt, _ ptr2: UnsafeMutablePointer<CInt>?, _ len2: CInt) -> UnsafeMutablePointer<CInt>? { nil }

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy ptr) @_lifetime(ptr: copy ptr) @_lifetime(ptr2: copy ptr2) @_disfavoredOverload
public func myFunc(_ ptr: inout MutableSpan<CInt>?, _ ptr2: inout MutableSpan<CInt>?) -> MutableSpan<CInt>? {
    let len = CInt(exactly: ptr?.count ?? 0)!
    let len2 = CInt(exactly: ptr2?.count ?? 0)!
    return unsafe _swiftifyOverrideLifetime({ () in
      let _resultValue = { () in
              return if ptr2 == nil {
                  { () in
                      return if ptr == nil {
                              unsafe myFunc(nil, len, nil, len2)
                            } else {
                              unsafe ptr!.withUnsafeMutableBufferPointer { _ptrPtr in
                                return unsafe myFunc(_ptrPtr.baseAddress, len, nil, len2)
                              }
                            }
                  }()
                } else {
                  unsafe ptr2!.withUnsafeMutableBufferPointer { _ptr2Ptr in
                    return { () in
                        return if ptr == nil {
                                unsafe myFunc(nil, len, _ptr2Ptr.baseAddress, len2)
                              } else {
                                unsafe ptr!.withUnsafeMutableBufferPointer { _ptrPtr in
                                  return unsafe myFunc(_ptrPtr.baseAddress, len, _ptr2Ptr.baseAddress, len2)
                                }
                              }
                    }()
                  }
                }
          }()
      if unsafe _resultValue == nil {
        return nil
      } else {
        return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())
      }
        }(), copying: ())
}
------------------------------
