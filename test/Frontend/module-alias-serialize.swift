/// Test serialized module contains the module alias when -module-alias flag is passed.
///
/// Module 'ClientN' imports 'Lib', and 'Lib' imports module 'Logging'.
/// 'Logging' needs to be aliased due to a name collision, so it's aliased 'AppleLogging'.

// RUN: %empty-directory(%t)

/// Create AppleLogging.swiftmodule by aliasing Logging via -module-alias Logging=AppleLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias Logging=AppleLogging %S/Inputs/module_aliasing/Logging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule

/// Check AppleLogging.swiftmodule is created
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/Logging.swiftmodule

/// Check AppleLogging.swiftmodule correctly contains AppleLogging as module name in the binary
// RUN: llvm-bcanalyzer --dump %t/AppleLogging.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER-NAME
// BCANALYZER-NAME: MODULE_NAME{{.*}}AppleLogging

/// Create module Lib that imports Logging with -module-alias Logging=AppleLogging
// RUN: %target-swift-frontend -module-name Lib %S/Inputs/module_aliasing/Lib.swift -module-alias Logging=AppleLogging -I %t -emit-module -emit-module-path %t/Lib.swiftmodule -Rmodule-loading 2> %t/result-Lib.output

/// Check Lib.swiftmodule is created
// RUN: test -f %t/Lib.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/Logging.swiftmodule

/// Check AppleLogging.swiftmodule is loaded
// RUN: %FileCheck %s -input-file %t/result-Lib.output -check-prefix CHECK-LOAD1
// CHECK-LOAD1: remark: loaded module at {{.*}}AppleLogging.swiftmodule

/// Check Lib.swiftmodule contains AppleLogging in the binary
// RUN: llvm-bcanalyzer --dump %t/Lib.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER-IMPORT1
// BCANALYZER-IMPORT1: IMPORTED_MODULE{{.*}}AppleLogging

/// Create a module Client that imports Lib, without module aliasing for Logging
// RUN: %target-swift-frontend -module-name Client %S/Inputs/module_aliasing/Client.swift -I %t -emit-module -emit-module-path %t/Client.swiftmodule -Rmodule-loading 2> %t/result-Client.output

/// Check Client1.swiftmodule is created and Lib.swiftmodule and AppleLogging.swiftmodule are loaded
// RUN: test -f %t/Client.swiftmodule
// RUN: test -f %t/Lib.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/Logging.swiftmodule

// RUN: %FileCheck %s -input-file %t/result-Client.output -check-prefix CHECK-LOAD2
// CHECK-LOAD2: remark: loaded module at {{.*}}Lib.swiftmodule

/// Check Client.swiftmodule contains AppleLogging in the binary
// RUN: llvm-bcanalyzer --dump %t/Client.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER-IMPORT2
// BCANALYZER-IMPORT2: IMPORTED_MODULE{{.*}}Lib
