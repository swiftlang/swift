// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -verify-ignore-unrelated %s -DCHECK_SCOPING
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -module-name submodules
// RUN: echo 'import submodules; let s = "\(x), \(y)"' | %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck - -I %t
// RUN: echo 'import submodules; let s = "\(x), \(y)"' | not %target-swift-frontend -typecheck - -I %t 2>&1 | %FileCheck -check-prefix=MISSING %s

import typealias ctypes.bits.DWORD
// MISSING: missing required modules:
// MISSING-DAG: 'ctypes.bits'
// MISSING-DAG: 'ctypes'

// From bits submodule
public var x : DWORD = 0
public var y : CInt = x

let _: ctypes.DWORD = 0

func markUsed<T>(_ t: T) {}

#if CHECK_SCOPING
markUsed(MY_INT) // expected-error {{cannot find 'MY_INT' in scope}}
markUsed(ctypes.MY_INT) // expected-error {{module 'ctypes' has no member named 'MY_INT'}}
let _: ctypes.Color? = nil // expected-error {{no type named 'Color' in module 'ctypes'}}
#endif
