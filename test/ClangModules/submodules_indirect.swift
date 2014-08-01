// RUN: rm -rf %t && mkdir -p %t
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -I %S/Inputs/custom-modules/ %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: ctypes{{.*}}.pcm

// RUN: %swift %clang-importer-sdk -emit-module-path %t/submodules.swiftmodule -module-cache-path %t/clang-module-cache -I %S/Inputs/custom-modules/ %s
// RUN: echo 'import submodules; println("\(x), \(y)")' | %swift %clang-importer-sdk -parse - -I %t -I %S/Inputs/custom-modules/
// RUN: echo 'import submodules; println("\(x), \(y)")' | not %swift -parse - -I %t -I %S/Inputs/custom-modules/ 2>&1 | FileCheck -check-prefix=MISSING %s

import ctypes_bits_exported
// MISSING: could not build Objective-C module 'ctypes_bits_exported'

// From bits submodule
public var x : DWORD = MY_INT
public var y : CInt = x
