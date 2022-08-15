// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module -module-name Core -o %t
// RUN: %target-swift-frontend -parse-as-library %t/Core.swiftmodule -typecheck -module-name Core -clang-header-expose-public-decls -emit-clang-header-path %t/core.h
// RUN: %FileCheck %s < %t/core.h

// RUN: %empty-directory(%t-evo)
// RUN: %target-swift-frontend %s -emit-module -enable-library-evolution -module-name Core -o %t-evo
// RUN: %target-swift-frontend -parse-as-library %t-evo/Core.swiftmodule -enable-library-evolution -typecheck -module-name Core -clang-header-expose-public-decls -emit-clang-header-path %t-evo/core.h
// RUN: %FileCheck %s < %t-evo/core.h


public func reprintedInImportedModule() -> Int {
    return 42
}

// CHECK: namespace Core {
// CHECK: swift::Int reprintedInImportedModule() noexcept SWIFT_WARN_UNUSED_RESULT {
