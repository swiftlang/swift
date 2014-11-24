// RUN: rm -rf %t && mkdir -p %t
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -I %S/Inputs/custom-modules/ %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// XFAIL: linux
// CHECK: ctypes{{.*}}.pcm

// RUN: %swift %clang-importer-sdk -o %t -emit-module -module-cache-path %t/clang-module-cache -I %S/Inputs/custom-modules/ %s -module-name submodules
// RUN: echo 'import submodules; println("\(x), \(y)")' | %swift %clang-importer-sdk -parse - -I %t -I %S/Inputs/custom-modules/
// RUN: echo 'import submodules; println("\(x), \(y)")' | not %swift -parse - -I %t -I %S/Inputs/custom-modules/ 2>&1 | FileCheck -check-prefix=MISSING %s

import ctypes_bits_exported
// MISSING: could not build Objective-C module 'ctypes_bits_exported'

// From bits submodule
public var x : DWORD = MY_INT
public var y : CInt = x
