// RUN: rm -rf %t && mkdir -p %t
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: ctypes{{.*}}.pcm

// RUN: %swift %clang-importer-sdk -emit-module-path %t/submodules.swiftmodule -module-cache-path %t/clang-module-cache %s
// RUN: echo 'import submodules; println("\(x), \(y)")' | %swift %clang-importer-sdk -parse - -I %t
// RUN: echo 'import submodules; println("\(x), \(y)")' | not %swift -parse - -I %t 2>&1 | FileCheck -check-prefix=MISSING %s

import ctypes
import ctypes.bits
// MISSING: missing required modules:
// MISSING-DAG: 'ctypes.bits'
// MISSING-DAG: 'ctypes'

// From bits submodule
public var x : DWORD = MY_INT
public var y : CInt = x
