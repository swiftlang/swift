// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -module-name Logging -package-name MyLoggingPkg %t/File.swift -emit-module -emit-module-path %t/Logging.swiftmodule
// RUN: test -f %t/Logging.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/Logging.swiftmodule | %FileCheck %s -check-prefix CHECK-BLOB
// CHECK-BLOB: <MODULE_PACKAGE_NAME abbrevid=5/> blob data = 'MyLoggingPkg'

// RUN: %target-swift-frontend -module-name Logging -package-name MyLoggingPkg %t/File.swift -emit-module -emit-module-interface-path %t/Logging.swiftinterface -swift-version 5 -enable-library-evolution -I %t
// RUN: test -f %t/Logging.swiftinterface
// RUN: %FileCheck %s -input-file %t/Logging.swiftinterface -check-prefix CHECK-FLAG
// CHECK-FLAG: -package-name MyLoggingPkg

// BEGIN File.swift
public func log(level: Int) {}
