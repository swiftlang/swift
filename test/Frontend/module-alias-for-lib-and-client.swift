/// Test the -module-alias flag on the following scenario:
/// Module 'Lib' imports module 'Logging', and module 'ClientN' imports both 'Lib' and 'Logging'.
/// 'Logging' needs to be aliased due to a name collision, so is renamed 'AppleLogging'.

// RUN: %empty-directory(%t)

/// Input file with a reference to its enclosing module called Logging
/// Create AppleLogging.swiftmodule by aliasing Logging via -module-alias Logging=AppleLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias Logging=AppleLogging %S/Inputs/module_aliasing/Logging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule

/// Check AppleLogging.swiftmodule is created
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/Logging.swiftmodule

/// Create a module Lib that imports Logging with -module-alias Logging=AppleLogging
// RUN: echo 'import Logging' > %t/FileLib.swift
// RUN: echo 'public func start() { Logging.setup() }' >> %t/FileLib.swift
// RUN: %target-swift-frontend -module-name Lib %S/Inputs/module_aliasing/Lib.swift -module-alias Logging=AppleLogging -I %t -emit-module -emit-module-path %t/Lib.swiftmodule -Rmodule-loading 2> %t/result-Lib.output

/// Check Lib.swiftmodule is created and AppleLogging.swiftmodule is loaded
// RUN: test -f %t/Lib.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/Logging.swiftmodule

// RUN: %FileCheck %s -input-file %t/result-Lib.output -check-prefix CHECK-Lib
// CHECK-Lib: remark: loaded module at {{.*}}AppleLogging.swiftmodule

/// Create module Client1 that imports both Lib and Logging, with module aliasing for Logging
// RUN: %target-swift-frontend -module-name Client1 %S/Inputs/module_aliasing/Client_imports_Lib_and_Logging.swift -module-alias Logging=AppleLogging -I %t -emit-module -emit-module-path %t/Client1.swiftmodule -Rmodule-loading 2>&1 | %FileCheck %s -check-prefix CHECK-1

/// Check Client1.swiftmodule is created and Lib.swiftmodule and AppleLogging.swiftmodule are loaded
// RUN: test -f %t/Client1.swiftmodule
// RUN: test -f %t/Lib.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/Logging.swiftmodule
// CHECK-1: remark: loaded module at {{.*}}AppleLogging.swiftmodule
// CHECK-1: remark: loaded module at {{.*}}Lib.swiftmodule

/// Try creating module Client2 that imports both Lib and Logging, without module aliasing
// RUN: not %target-swift-frontend -module-name Client2 %S/Inputs/module_aliasing/Client_imports_Lib_and_Logging.swift -I %t -emit-module -emit-module-path %t/Client2.swiftmodule 2> %t/result-Client2.output

/// Check that it fails
// RUN: %FileCheck %s -input-file %t/result-Client2.output -check-prefix CHECK-2
// CHECK-2: {{.*}}error: no such module 'Logging'

/// Create module Client3 that imports both Lib and AppleLogging, without module aliasing
// RUN: %target-swift-frontend -module-name Client3 %S/Inputs/module_aliasing/Client_imports_Lib_and_AppleLogging.swift -I %t -emit-module -emit-module-path %t/Client3.swiftmodule -Rmodule-loading 2>&1 | %FileCheck %s -check-prefix CHECK-3

/// Check Client3.swiftmodule is created and correct modules are loaded
// RUN: test -f %t/Client3.swiftmodule
// RUN: test -f %t/Lib.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/Logging.swiftmodule
// CHECK-3: remark: loaded module at {{.*}}AppleLogging.swiftmodule
// CHECK-3: remark: loaded module at {{.*}}Lib.swiftmodule
