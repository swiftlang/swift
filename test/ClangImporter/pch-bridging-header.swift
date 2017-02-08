// REQUIRES: objc_interop
// RUN: rm -rf %t && mkdir -p %t/tmp

// First test the explicit frontend-based bridging PCH generation and use works
// RUN: %target-swift-frontend -emit-pch -o %t/sdk-bridging-header.pch %S/Inputs/sdk-bridging-header.h
// RUN: %target-swift-frontend -parse -verify %s -import-objc-header %t/sdk-bridging-header.pch

// Now test the driver-automated version is inert when disabled
// RUN: env TMPDIR=%t/tmp/ %target-swiftc_driver -parse -disable-bridging-pch -save-temps %s -import-objc-header %S/Inputs/sdk-bridging-header.h
// RUN: not ls %t/tmp/*.pch >/dev/null 2>&1

// Test the driver-automated version works by default
// RUN: env TMPDIR=%t/tmp/ %target-swiftc_driver -parse -save-temps %s -import-objc-header %S/Inputs/sdk-bridging-header.h
// RUN: ls %t/tmp/*.pch >/dev/null 2>&1
// RUN: llvm-objdump -raw-clang-ast %t/tmp/*.pch | llvm-bcanalyzer -dump | %FileCheck %s
// CHECK: ORIGINAL_FILE{{.*}}Inputs/sdk-bridging-header.h

// Test the driver-automated version deletes its PCH file when done
// RUN: rm %t/tmp/*.pch
// RUN: env TMPDIR=%t/tmp/ %target-swiftc_driver -parse %s -import-objc-header %S/Inputs/sdk-bridging-header.h
// RUN: not ls %t/tmp/*.pch >/dev/null 2>&1

import Foundation

let not = MyPredicate.not()
let and = MyPredicate.and([])
let or = MyPredicate.or([not, and])

