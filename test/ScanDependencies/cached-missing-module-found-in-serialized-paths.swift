// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/deps)
// RUN: %empty-directory(%t/moreDeps)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/deps/B.swiftmodule -module-cache-path %t/module-cache %t/B.swift -module-name B -I %t/moreDeps

// Put the dependency module into a location discoverable by the first scan which will succeed and serialize scanner state
// RUN: cp %t/moreDeps/C.swiftinterface %t/deps/C.swiftinterface

// RUN: %target-swift-frontend -scan-dependencies -o %t/deps.json %t/client.swift -I %t/deps -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -Rdependency-scan-cache -serialize-dependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache &> %t/initial_output.txt

// Remove the 'C' dependency module into a location not discoverable by the second scan in order to trigger a failure and use serialized scanner state
// to emit the diagnostic
// RUN: rm %t/deps/C.swiftinterface

// RUN: %target-swift-frontend -scan-dependencies -o %t/deps.json %t/client.swift -I %t/deps -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -validate-prior-dependency-scan-cache &> %t/output.txt
// RUN: cat %t/output.txt | %FileCheck %s

// CHECK: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: '{{.*}}cache.moddepcache'.
// CHECK: remark: Incremental module scan: Dependency info for module 'C' invalidated due to a modified input since last scan: '{{.*}}deps{{/|\\}}C.swiftinterface'.
// CHECK: remark: Incremental module scan: Dependency info for module 'deps' invalidated due to an out-of-date dependency.
// CHECK: error: Compilation search paths unable to resolve module dependency: 'C'
// CHECK: note: 'C' can be found using a search path that was specified when building module 'B' ('{{.*}}moreDeps'). This search path was not explicitly specified on the current compilation.
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
public func funcB() {}

//--- client.swift
import A
import B
import C
