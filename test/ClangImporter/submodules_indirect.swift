// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -verify -I %S/Inputs/custom-modules/ %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -o %t -emit-module -I %S/Inputs/custom-modules/ %s -module-name submodules
// RUN: echo 'import submodules; let s = "\(x), \(y)"' | %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck - -I %t -I %S/Inputs/custom-modules/
// RUN: echo 'import submodules; let s = "\(x), \(y)"' | not %target-swift-frontend -enable-objc-interop -typecheck - -I %t -I %S/Inputs/custom-modules/ 2>&1 | %FileCheck -check-prefix=MISSING %s

import ctypes_bits_exported
// MISSING: could not build Objective-C module 'ctypes_bits_exported'

// From bits submodule
public var x : DWORD = MY_INT
public var y : CInt = x
