// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -verify -F %S/Inputs/objc_protocol_module_visibility -import-objc-header %S/Inputs/objc_protocol_module_visibility/bridging.h %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-pch -F %S/Inputs/objc_protocol_module_visibility -o %t.pch %S/Inputs/objc_protocol_module_visibility/bridging.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -verify -F %S/Inputs/objc_protocol_module_visibility -import-pch %t.pch %s

// REQUIRES: objc_interop

// An ObjC protocol or class redeclared both non-modularly (via the bridging
// header) and inside a real framework module must still be found by a
// module-qualified lookup into that framework, unless it is a forward
// declaration.

import ModuleA

typealias X = ModuleA.RedeclaredInModuleA
typealias Y = ModuleA.ForwardRedeclaredInModuleA // expected-error {{no type named 'ForwardRedeclaredInModuleA' in module 'ModuleA'}}

typealias Z = ModuleA.RedeclaredClassInModuleA
typealias W = ModuleA.ForwardRedeclaredClassInModuleA // expected-error {{no type named 'ForwardRedeclaredClassInModuleA' in module 'ModuleA'}}
