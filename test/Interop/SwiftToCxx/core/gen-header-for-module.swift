// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module -module-name Core -o %t
// RUN: %target-swift-frontend -parse-as-library %t/Core.swiftmodule -module-name Core -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/core.h
// RUN: %FileCheck %s < %t/core.h

// RUN: %empty-directory(%t-evo)
// RUN: %target-swift-frontend %s -emit-module -enable-library-evolution -module-name Core -o %t-evo
// RUN: %target-swift-frontend -parse-as-library %t-evo/Core.swiftmodule -enable-library-evolution -module-name Core -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t-evo/core.h
// RUN: %FileCheck %s < %t-evo/core.h

// RUN: %empty-directory(%t-int)
// RUN: %target-swift-frontend %s -typecheck -emit-module-interface-path %t-int/Core.swiftinterface -module-name Core
// RUN: %target-swift-frontend -parse-as-library %t-int/Core.swiftinterface -module-name Core -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t-int/core.h
// RUN: %FileCheck %s < %t-int/core.h

// RUN: %empty-directory(%t-int-evo)
// RUN: %target-swift-frontend %s -typecheck -enable-library-evolution -emit-module-interface-path %t-int-evo/Core.swiftinterface -module-name Core
// RUN: %target-swift-frontend -parse-as-library %t-int-evo/Core.swiftinterface -enable-library-evolution -module-name Core -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t-int-evo/core.h
// RUN: %FileCheck %s < %t-int-evo/core.h

public func reprintedInImportedModule() -> Int {
    return 42
}

// CHECK: namespace Core SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Core") {
// CHECK: swift::Int reprintedInImportedModule() noexcept SWIFT_SYMBOL("s:4Core25reprintedInImportedModuleSiyF") SWIFT_WARN_UNUSED_RESULT {
