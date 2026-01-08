// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-name A -o %t/include/A.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -emit-module-interface-path %t/include/A.swiftinterface -enable-library-evolution -I %t/internal -enable-testing \
// RUN:   %t/A.swift

// RUN: %swift-scan-test -action scan_dependency -cwd %t -- %target-swift-frontend -emit-module -module-name Test \
// RUN:   -swift-version 5 -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -I include %t/test.swift | %FileCheck %s

// CHECK: "mainModuleName": "Test"

// RUN: cd %t
// RUN: %swift-scan-test -action scan_dependency -- %target-swift-frontend -emit-module -module-name Test \
// RUN:   -swift-version 5 -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -I include %t/test.swift | %FileCheck %s

//--- test.swift
import A

//--- A.swift
public func a() {}
