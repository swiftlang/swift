// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %target-swift-frontend %clang-importer-sdk -parse -verify %s -DIMPORT_TOP_LEVEL
// RUN: %target-swift-frontend %clang-importer-sdk -parse -verify %s

// RUN: %target-swift-frontend %clang-importer-sdk -emit-module-path %t/submodules.swiftmodule %s -DNO_ERRORS
// RUN: echo 'import submodules; println("\(x), \(y)")' | %target-swift-frontend %clang-importer-sdk -parse - -I %t
// RUN: echo 'import submodules; println("\(x), \(y)")' | not %target-swift-frontend -parse - -I %t 2>&1 | FileCheck -check-prefix=MISSING %s

// XFAIL: linux

#if IMPORT_TOP_LEVEL
import ctypes
#endif

import ctypes.bits
// MISSING: missing required modules:
// MISSING-DAG: 'ctypes.bits'
// MISSING-DAG: 'ctypes'

// From bits submodule
public var x : DWORD = MY_INT
public var y : CInt = x

let _: ctypes.DWORD = ctypes.MY_INT
let _: ctypes.Color? = nil

// Error: "bits" should not be a valid name in this scope.
#if !NO_ERRORS
let _: bits.DWORD = 0 // expected-error {{use of undeclared type 'bits'}}
#endif
