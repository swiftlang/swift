// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/cdecl_implementation.swiftinterface) %s -import-underlying-module %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/cdecl_implementation
// RUN: %FileCheck --input-file %t/cdecl_implementation.swiftinterface %s
// RUN: %FileCheck --input-file %t/cdecl_implementation.swiftinterface --check-prefix NEGATIVE %s
// RUN: %target-swift-typecheck-module-from-interface(%t/cdecl_implementation.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/cdecl_implementation

// We should never see @_objcImplementation in the header
// NEGATIVE-NOT: @_objcImplementation

//
// @_objcImplementation @_cdecl func
//

// CHECK-NOT: func implFunc
@_objcImplementation @_cdecl("implFunc")
public func implFunc(_: Int32) {}
