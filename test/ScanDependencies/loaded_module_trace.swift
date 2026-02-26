// REQUIRES: swift_feature_RegionBasedIsolation
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/cache)
// RUN: split-file %s %t

// RUN: %target-build-swift -emit-module -module-name Module %S/../Driver/Inputs/loaded_module_trace_empty.swift \
// RUN:   -o %t/Module.swiftmodule -module-cache-path %t/cache
// RUN: %target-build-swift -emit-module -module-name Module2 %S/../Driver/Inputs/loaded_module_trace_imports_module.swift \
// RUN:   -o %t/Module2.swiftmodule -I %t -module-cache-path %t/cache -strict-memory-safety
// RUN: %target-build-swift -emit-library -module-name Plugin %S/../Driver/Inputs/loaded_module_trace_compiler_plugin.swift \
// RUN:   -o %t/%target-library-name(Plugin) -module-cache-path %t/cache

// RUN: %target-swift-frontend -scan-dependencies %t/test.swift -emit-loaded-module-trace -emit-loaded-module-trace-path %t/trace.json \
// RUN:   -enable-upcoming-feature RegionBasedIsolation -strict-memory-safety -module-name Test -dependency-only-import B \
// RUN:   -I %t -module-cache-path %t/cache -load-plugin-library %t/%target-library-name(Plugin) \
// RUN:   -serialize-dependency-scan-cache -dependency-scan-cache-path %t/scan-cache -o %t/deps.json

// RUN: %FileCheck %s < %t/trace.json

// RUN: %target-swift-frontend -scan-dependencies %t/test.swift -emit-loaded-module-trace -emit-loaded-module-trace-path %t/trace2.json \
// RUN:   -enable-upcoming-feature RegionBasedIsolation -strict-memory-safety -module-name Test -dependency-only-import B \
// RUN:   -I %t -module-cache-path %t/cache -load-plugin-library %t/%target-library-name(Plugin) \
// RUN:   -load-dependency-scan-cache -dependency-scan-cache-path %t/scan-cache -o %t/deps2.json

// RUN: diff %t/trace.json %t/trace2.json

// CHECK: "swiftmodulesDetailedInfo":[
// CHECK-DAG: {"name":"Module2","path":"{{[^"]*\\[/\\]}}Module2.swiftmodule","isImportedDirectly":true,"supportsLibraryEvolution":false,"strictMemorySafety":true}
// CHECK-DAG: {"name":"Swift","path":"{{[^"]*\\[/\\]}}Swift.swiftmodule{{(\\[/\\][^"]+[.]swiftinterface)?}}","isImportedDirectly":true,"supportsLibraryEvolution":true,"strictMemorySafety":true}
// CHECK-DAG: {"name":"SwiftOnoneSupport","path":"{{[^"]*\\[/\\]}}SwiftOnoneSupport.swiftmodule{{(\\[/\\][^"]+[.]swiftinterface)?}}","isImportedDirectly":true,"supportsLibraryEvolution":true,"strictMemorySafety":true}
// CHECK-DAG: {"name":"Module","path":"{{[^"]*\\[/\\]}}Module.swiftmodule","isImportedDirectly":false,"supportsLibraryEvolution":false,"strictMemorySafety":false}
// CHECK-DAG: {"name":"A","path":"{{[^"]*\\[/\\]}}A.swiftinterface","isImportedDirectly":true,"supportsLibraryEvolution":true,"strictMemorySafety":true}
// CHECK: ],
// CHECK: "swiftmacros":[
// CHECK-DAG: {"name":"Plugin","path":"{{[^"]*\\[/\\]}}{{libPlugin.dylib|libPlugin.so|Plugin.dll}}"}
// CHECK: ]
// CHECK: }
// CHECK-NOT: "B"

//--- test.swift
import Module2
import A

@freestanding(expression) macro echo<T>(_: T) -> T = #externalMacro(module: "Plugin", type: "EchoMacro")

//--- A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A
// swift-module-flags-ignorable: -strict-memory-safety
public func funcA() { }

//--- B.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name B
public func funcB() { }
