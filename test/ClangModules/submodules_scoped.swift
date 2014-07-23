// RUN: rm -rf %t && mkdir -p %t
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache %s -DCHECK_SCOPING
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: ctypes{{.*}}.pcm

// RUN: %swift %clang-importer-sdk -emit-module-path %t/submodules.swiftmodule -module-cache-path %t/clang-module-cache %s
// RUN: echo 'import submodules; println("\(x), \(y)")' | %swift %clang-importer-sdk -parse - -I %t
// RUN: echo 'import submodules; println("\(x), \(y)")' | not %swift -parse - -I %t 2>&1 | FileCheck -check-prefix=MISSING %s

import typealias ctypes.bits.DWORD
// MISSING: missing required modules:
// MISSING-DAG: 'ctypes.bits'
// MISSING-DAG: 'ctypes'

// From bits submodule
public var x : DWORD = 0
public var y : CInt = x

let _: ctypes.DWORD = 0

#if CHECK_SCOPING
println(MY_INT) // expected-error {{use of unresolved identifier 'MY_INT'}}
println(ctypes.MY_INT) // expected-error {{module 'ctypes' has no member named 'MY_INT'}}
let _: ctypes.Color? = nil // expected-error {{no type named 'Color' in module 'ctypes'}}
#endif
