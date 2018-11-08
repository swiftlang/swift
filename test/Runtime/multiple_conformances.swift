// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library -force-single-frontend-invocation %S/Inputs/RetroactiveCommon.swift -emit-module-path %t/RetroactiveCommon.swiftmodule -emit-object -o %t/RetroactiveCommon.o
// RUN: %target-build-swift -parse-as-library -force-single-frontend-invocation -I %t %S/Inputs/RetroactiveA.swift -emit-module-path %t/RetroactiveA.swiftmodule -emit-object -o %t/RetroactiveA.o
// RUN: %target-build-swift -parse-as-library -force-single-frontend-invocation -I %t %S/Inputs/RetroactiveB.swift -emit-module-path %t/RetroactiveB.swiftmodule -emit-object -o %t/RetroactiveB.o
// RUN: %target-build-swift -I %t %s %t/RetroactiveCommon.o %t/RetroactiveA.o %t/RetroactiveB.o -module-name main -o %t/a.out 
// RUN: %target-codesign %t/a.out
// RUN: env SWIFT_ENABLE_CONFLICTING_CONFORMANCES_CHECK=1 %target-run %t/a.out > %t.log 2>&1
// RUN: %FileCheck %s < %t.log

// REQUIRES: executable_test

import RetroactiveCommon
import RetroactiveA
import RetroactiveB

// CHECK: Swift runtime warning: multiple conformances for 'RetroactiveCommon.CommonStruct: CommonP1' found in modules 'RetroactiveA' and 'RetroactiveB'. Arbitrarily selecting conformance from module 'RetroactiveA'
let demangled1 = _typeByName("17RetroactiveCommon16CommonRequiresP1Vy17RetroactiveCommon12CommonStructVG")!
