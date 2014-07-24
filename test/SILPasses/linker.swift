// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module %S/Inputs/linker_pass_input.swift -o %t/Swift.swiftmodule -parse-stdlib -parse-as-library -module-name Swift -sil-serialize-all -module-link-name swiftCore
// RUN: %swift %s -O -I=%t -sil-debug-serialization -o - -emit-sil | FileCheck %s

// CHECK: sil public_external @_TFSs11doSomethingFT_T_ : $@thin () -> () {

doSomething()
