// RUN: %empty-directory(%t)

// A module should fail to be generated if the same filename is used twice and '-experimental-allow-module-with-compiler-errors' is not passed

// RUN: not %target-swift-frontend -emit-module -o %t/no_allow_compiler_errors.swiftmodule %S/Inputs/same_filename/A/File.swift %S/Inputs/same_filename/B/File.swift
// RUN: not ls %t/no_allow_compiler_errors.swiftmodule

// If '-experimental-allow-module-with-compiler-errors' is passed, we should throw an error but still generate a module

// RUN: %target-swift-frontend -emit-module -experimental-allow-module-with-compiler-errors -o %t/allow_compiler_errors.swiftmodule %S/Inputs/same_filename/A/File.swift %S/Inputs/same_filename/B/File.swift 2>&1 | %FileCheck %s
// RUN: ls %t/allow_compiler_errors.swiftmodule

// CHECK: filename "File.swift" used twice:
