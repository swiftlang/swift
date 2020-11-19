// RUN: %empty-directory(%t)
// RUN: not --crash %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -enable-testing -module-name NonModularApp -emit-module -o %t/NonModularApp.swiftmodule -import-objc-header %S/Inputs/non-modular-header.h -DNON_MODULAR_APP -use-clang-function-types 2>&1 | %FileCheck %s

// CHECK: Clang function type is not serializable

#if NON_MODULAR_APP
import ctypes
struct S {
  static func f(_ : @convention(c, cType: "void (*)(PlaceholderType, size_t)") (PlaceholderType, Int) -> ()) {}
}
#endif
