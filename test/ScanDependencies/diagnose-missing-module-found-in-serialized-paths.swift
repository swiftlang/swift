// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/deps)
// RUN: %empty-directory(%t/moreDeps)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/deps/B.swiftmodule -module-cache-path %t/module-cache %t/B.swift -module-name B -I %t/moreDeps

// RUN: %target-swift-frontend -scan-dependencies -o %t/deps.json %t/client.swift -I %t/deps -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import &> %t/output.txt
// RUN: cat %t/output.txt | %FileCheck %s

// CHECK: error: compilation search paths unable to resolve module dependency: 'C'
// CHECK: note: 'C' can be found using a search path that was specified when building module 'B' ('{{.*}}moreDeps'). This search path was not explicitly specified on the current compilation
// CHECK: note: a dependency of Swift module 'B': '{{.*}}B.swiftmodule'
// CHECK: note: a dependency of main module 'deps'

//--- moreDeps/C.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name C -enable-library-evolution
public struct structC {}

//--- deps/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -enable-library-evolution
public func funcA() {}

//--- B.swift
public import C
public func funcB(_ input: structC) {}

//--- client.swift
import A
import B

