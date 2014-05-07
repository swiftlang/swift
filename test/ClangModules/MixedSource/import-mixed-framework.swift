// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: cp -r %S/Inputs/mixed-framework/Mixed.framework %t

// Don't crash if a generated header is present but the swiftmodule is missing.
// RUN: not %swift %clang-importer-sdk -module-cache-path %t -F %t -parse %s

// RUN: %swift_driver -emit-module -o %t/Mixed.framework/Modules/Mixed.swiftmodule/x86_64.swiftmodule %S/Inputs/mixed-framework/Mixed.swift -module-name Mixed -Xfrontend -disable-objc-attr-requires-objc-module
// RUN: %swift %clang-importer-sdk -module-cache-path %t -F %t -parse %s -verify

// REQUIRES: X86

import Mixed

let instance = SwiftClass(x: 42)
instance.pureSwiftMethod(nil)

let clangStruct = PureClangType(x: 1, y: 2)
instance.categoryMethod(clangStruct)

let x: BogusClass? = nil // expected-error {{'BogusClass' is unavailable: cannot find Swift declaration for this class}}
