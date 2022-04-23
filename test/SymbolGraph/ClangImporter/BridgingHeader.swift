// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-pch -pch-output-dir %t %S/Inputs/ObjcProperty/ObjcProperty.framework/Headers/ObjcProperty.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module-path %t/BridgingHeader.swiftmodule -import-objc-header %S/Inputs/ObjcProperty/ObjcProperty.framework/Headers/ObjcProperty.h -pch-output-dir %t -module-name BridgingHeader -disable-objc-attr-requires-foundation-module %s -emit-symbol-graph -emit-symbol-graph-dir %t -symbol-graph-minimum-access-level internal

// RUN: %FileCheck %s --input-file %t/BridgingHeader.symbols.json

// REQUIRES: objc_interop

// There are a few implicit symbols from Clang that snuck in when building with minimum-access
// level of "internal". Make sure they don't sneak back in. rdar://92018648

// CHECK-NOT: __NSConstantString
// CHECK-NOT: __builtin_ms_va_list
// CHECK-NOT: __builtin_va_list

