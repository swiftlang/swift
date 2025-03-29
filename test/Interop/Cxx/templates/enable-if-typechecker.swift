// RUN: not %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop -enable-objc-interop 2>&1 | %FileCheck %s
// REQUIRES: objc_interop

import StdlibUnittest
import EnableIf


let x = HasMethodWithEnableIf()
x.onlyEnabledForBool("a")
// CHECK: error: could not generate C++ types from the generic Swift types provided; the following Swift type(s) provided to 'HasMethodWithEnableIf::onlyEnabledForBool' could not be converted: String
