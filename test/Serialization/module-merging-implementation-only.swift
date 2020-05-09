// This is a very simple test that module merging does not eliminate
// @_implementationOnly imports or declarations referenced from those imports.
// More thorough tests exist in LLDB, which can look into those imports when
// debugging a client of the module with @_implementationOnly imports.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_struct.swift

// RUN: %target-swift-frontend -emit-module -I %t -o %t/main~partial.swiftmodule -module-name main %s
// RUN: llvm-bcanalyzer -dump %t/main~partial.swiftmodule | %FileCheck %s
// RUN: grep -q TwoInts %t/main~partial.swiftmodule

// RUN: %target-swift-frontend -merge-modules -emit-module -I %t -o %t/main.swiftmodule %t/main~partial.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/main.swiftmodule | %FileCheck %s
// RUN: grep -q TwoInts %t/main.swiftmodule

@_implementationOnly import def_struct

struct Container {
  var wrapped: TwoInts
}

// CHECK: <IMPORTED_MODULE abbrevid={{[0-9]+}} op0=2 op1=0{{.*}}/> blob data = 'def_struct'
