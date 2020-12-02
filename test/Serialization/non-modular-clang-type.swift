// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -enable-testing -module-name NonModularApp -emit-module -o %t/NonModularApp.swiftmodule -import-objc-header %S/Inputs/non-modular-header.h -DNON_MODULAR_APP -use-clang-function-types
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -module-name NonModularAppTest -I %t -DNON_MODULAR_APP_TEST

// CHECK: Clang function type is not serializable

#if NON_MODULAR_APP
import ctypes
struct S {
  static func f(_ : @convention(c, cType: "void (*)(PlaceholderType, size_t)") (PlaceholderType, Int) -> ()) {}
}
#endif

#if NON_MODULAR_APP_TEST
@testable import NonModularApp

S.f({ _, _ in })
#endif
