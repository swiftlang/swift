// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/CrossCompiler.framework %t/
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %S/../Inputs/empty.swift -typecheck -emit-objc-header-path %t/CrossCompiler.framework/Headers/empty.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module-path %t/CrossCompiler.framework/Modules/CrossCompiler.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t -F %clang-importer-sdk-path/frameworks -module-name CrossCompiler -disable-objc-attr-requires-foundation-module -parse-as-library %s -Xcc -Werror

// Ensure that building with multiple compatibility headers from different compilers doesn't
// trigger "macro redefinition" errors (rdar://106087804)

// CrossCompiler.framework contains a modified header generated with Swift 5.7 (before the
// PrintAsClang refactor to make macros data-driven). By compiling it alongside a freshly-generated
// header, we can make sure that the macro definitions are the same before and after the switch.

