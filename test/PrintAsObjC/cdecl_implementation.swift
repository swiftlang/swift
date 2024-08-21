// Please keep this file in alphabetical order!

// Temporarily disable on arm64e (rdar://127675057)
// UNSUPPORTED: CPU=arm64e

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -enable-experimental-feature CImplementation -I %S/Inputs/custom-modules -import-underlying-module -o %t %s -disable-objc-attr-requires-foundation-module -target %target-stable-abi-triple
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -parse-as-library %t/cdecl_implementation.swiftmodule -typecheck -enable-experimental-feature CImplementation -I %S/Inputs/custom-modules -emit-clang-header-path %t/cdecl_implementation-Swift.h -import-underlying-module -disable-objc-attr-requires-foundation-module -target %target-stable-abi-triple
// RUN: %FileCheck --check-prefix=NEGATIVE %s --input-file %t/cdecl_implementation-Swift.h
// RUN: %check-c-header-in-clang -I %S/Inputs/custom-modules/ %t/cdecl_implementation-Swift.h
// RUN: %check-c-header-in-clang -I %S/Inputs/custom-modules/ -fno-modules -Qunused-arguments %t/cdecl_implementation-Swift.h

@_cdecl("CImplFunc") @_objcImplementation func CImplFunc() {}
// NEGATIVE-NOT: CImplFunc(
