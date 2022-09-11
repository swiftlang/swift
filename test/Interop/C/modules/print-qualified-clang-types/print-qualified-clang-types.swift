// Check that when qualifying Clang types with a module name, we choose a
// visible module. Clang types need special treatment because multiple Clang
// modules can contain the same type declarations from a textually included
// header, but not all of these modules may be visible. If we choose a module
// that isn't visible, we produce `.swiftinterface` files that don't compile.
//
// To test this, the test sets up the following structure:
//
// MainModule (Swift module)
//   import HelperModule (Swift module)
//     import ForeignA (Clang module)
//       #include "textual-header.h"
//     @_exported import ForeignB (Clang module)
//       #include "textual-header.h"
//
// `ForeignA` and `ForeignB` both include the same textual header, which
// defines the struct `ForeignStruct`.
//
// Because `ForeignB` is re-exported by `HelperModule`, it is visible from
// `MainModule`, but `ForeignA` is not. This means that when `ForeignStruct` is
// used in `MainModule`, its qualified name should be printed as
// `ForeignB.ForeignStruct`, not `ForeignA.ForeignStruct`.
//
// In addition to checking for the presence of the expected string in the
// `.swiftinterface` file, we also verify that it compiles without error.
//
// This is a regression test for https://github.com/apple/swift/issues/55477.

// RUN: %empty-directory(%t)
// RUN: mkdir %t/helper_module %t/main_module
// RUN: %target-swift-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/helper_module/HelperModule.swiftmodule %S/Inputs/HelperModule.swift -I %S/Inputs
// RUN: %target-swift-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/main_module/MainModule.swiftmodule -emit-module-interface-path %t/main_module/MainModule.swiftinterface -I %t/helper_module %S/Inputs/MainModule.swift -I %S/Inputs
// RUN: %FileCheck --input-file=%t/main_module/MainModule.swiftinterface %s
// RUN: %target-swift-frontend -typecheck -swift-version 5 %t/main_module/MainModule.swiftinterface -I %t/helper_module -I %S/Inputs

// CHECK: public func funcTakingForeignStruct(_ param: ForeignB.ForeignStruct)
