// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -module-name Logging -package-name MyLoggingPkg %t/File.swift -emit-module -emit-module-path %t/Logging.swiftmodule
// RUN: test -f %t/Logging.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/Logging.swiftmodule | %FileCheck %s -check-prefix CHECK-BLOB
// CHECK-BLOB: <MODULE_PACKAGE_NAME abbrevid=5/> blob data = 'MyLoggingPkg'

// RUN: %target-swift-frontend -module-name Logging -package-name MyLoggingPkg %t/File.swift -emit-module -emit-module-interface-path %t/Logging.swiftinterface -emit-private-module-interface-path %t/Logging.private.swiftinterface -swift-version 5 -enable-library-evolution -I %t

// RUN: %target-swift-typecheck-module-from-interface(%t/Logging.swiftinterface) -I %t
// RUN: %FileCheck %s --check-prefix=CHECK-PUBLIC < %t/Logging.swiftinterface
// CHECK-PUBLIC: -module-name Logging
// CHECK-PUBLIC-NOT: -package-name

// RUN: %target-swift-typecheck-module-from-interface(%t/Logging.private.swiftinterface) -module-name Logging -I %t
// RUN: %FileCheck %s --check-prefix=CHECK-PRIVATE < %t/Logging.private.swiftinterface
// CHECK-PRIVATE: -module-name Logging
// CHECK-PRIVATE: swift-module-flags-ignorable-private: -package-name MyLoggingPkg

//--- File.swift
public func log(level: Int) {}
