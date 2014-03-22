// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: cp -r %S/Inputs/mixed-framework/Mixed.framework %t
// RUN: %swift_driver -target x86_64-apple-macosx10.9 -emit-module -o %t/Mixed.framework/Modules/Mixed.swiftmodule/x86_64.swiftmodule %S/Inputs/mixed-framework/Mixed.swift -module-name Mixed
// RUN: %swift %clang-importer-sdk -target x86_64-apple-macosx10.9 -module-cache-path %t -F %t -parse %s -verify
// REQUIRES: X86

import Mixed

let instance = SwiftClass(42)
instance.pureSwiftMethod(nil)

let clangStruct = PureClangType(1,2)
instance.categoryMethod(clangStruct)
