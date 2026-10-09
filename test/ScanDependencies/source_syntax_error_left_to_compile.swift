// A syntax error in one of the module's own source files is for the compile
// job to report, together with the semantic errors that a failed scan would
// hide. The scanner still records the file's imports.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -module-cache-path %t/clang-module-cache -I %S/Inputs/Swift -I %S/Inputs/CHeaders \
// RUN:   %t/main.swift -o %t/deps.json 2>&1 | %FileCheck %s --check-prefix=DIAGS --allow-empty
// RUN: %FileCheck %s --input-file=%t/deps.json

// DIAGS-NOT: error:

// CHECK: "mainModuleName": "Test"
// CHECK: "swift": "A"

//--- main.swift
import A

class Unclosed {
  func f() {}
