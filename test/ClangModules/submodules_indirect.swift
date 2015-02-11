// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -I %S/Inputs/custom-modules/ %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -o %t -emit-module -I %S/Inputs/custom-modules/ %s -module-name submodules
// RUN: echo 'import submodules; println("\(x), \(y)")' | %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse - -I %t -I %S/Inputs/custom-modules/
// RUN: echo 'import submodules; println("\(x), \(y)")' | not %target-swift-frontend -parse - -I %t -I %S/Inputs/custom-modules/ 2>&1 | FileCheck -check-prefix=MISSING %s

// XFAIL: linux

import ctypes_bits_exported
// MISSING: could not build Objective-C module 'ctypes_bits_exported'

// From bits submodule
public var x : DWORD = MY_INT
public var y : CInt = x
