// Test that there is no crash in such a case:
// - there is mixed framework A
// - swift module B depends on A and is built fine
// - there is a swift invocation that imports B but causes the ObjC part of A to fail to import


// RUN: rm -rf %t
// RUN: mkdir -p %t/MixModA.framework/Headers
// RUN: mkdir -p %t/MixModA.framework/Modules/MixModA.swiftmodule
// RUN: cp %S/Inputs/MixModA.modulemap %t/MixModA.framework/Modules/module.modulemap

// RUN: %target-swift-frontend -emit-module %S/Inputs/SwiftModA.swift -module-name MixModA -I %S/Inputs/objcfail -o %t/MixModA.framework/Modules/MixModA.swiftmodule/%target-swiftmodule-name -emit-objc-header -emit-objc-header-path %t/MixModA.framework/Headers/MixModA-Swift.h -module-cache-path %t/mcp
// RUN: %target-swift-frontend -emit-module %S/Inputs/SwiftModB.swift -module-name SwiftModB -F %t -o %t -module-cache-path %t/mcp

// RUN: %target-swift-frontend -typecheck %s -I %t -module-cache-path %t/mcp
// RUN: %target-swift-frontend -typecheck %s -Xcc -DFAIL -I %t -module-cache-path %t/mcp -show-diagnostics-after-fatal -verify -verify-ignore-unknown

// XFAIL: linux

import SwiftModB // expected-error {{missing required module}}
_ = TyB() // expected-error {{use of unresolved identifier 'TyB'}}

// -verify-ignore-unknown is for:
// <unknown>:0: error: unexpected error produced: could not build Objective-C module 'ObjCFail'
// <unknown>:0: error: unexpected error produced: missing required module 'ObjCFail'
