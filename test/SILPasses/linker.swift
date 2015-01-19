// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/linker_pass_input.swift -o %t/Swift.swiftmodule -parse-stdlib -parse-as-library -module-name Swift -sil-serialize-all -module-link-name swiftCore
// RUN: %target-swift-frontend %s -O -I %t -sil-debug-serialization -o - -emit-sil | FileCheck %s

// CHECK: sil public_external [fragile] @_TFSs11doSomethingFT_T_ : $@thin () -> () {
doSomething()

// Make sure we are not linking doSomethign2 because it is marked with 'noimport'

// CHECK: sil [semantics "stdlib_binary_only"] @_TFSs12doSomething2FT_T_ : $@thin () -> ()
// CHECK-NOT: return
doSomething2()

// CHECK: sil @unknown
