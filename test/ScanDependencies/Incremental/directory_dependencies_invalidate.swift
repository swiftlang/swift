// Test that Clang module directory dependencies are serialized in the scanner
// cache, and that reporting a changed directory invalidates a module loaded from
// it. Each swift-scan-test run is a separate scanner, as for a separate build,
// and writes its scanning modules to the module cache.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Initial scan, serializing the scanner cache.
// RUN: %swift-scan-test -action scan_dependency -- %target-swift-frontend -emit-module -module-name Test \
// RUN:   -module-cache-path %t/module-cache -parse-stdlib \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -I %t/include -Rdependency-scan \
// RUN:   -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   > %t/deps_initial.json 2> %t/remarks_initial.txt
// RUN: %FileCheck %s --check-prefix=REMARK-INITIAL < %t/remarks_initial.txt

// Add a header. Without a report the cached module is reused. Modules built in
// the same second a scan starts count as up to date, so wait first.
// RUN: sleep 1
// RUN: echo 'void added(void);' > %t/include/sub/added.h
// RUN: %swift-scan-test -action scan_dependency -- %target-swift-frontend -emit-module -module-name Test \
// RUN:   -module-cache-path %t/module-cache -parse-stdlib \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -I %t/include -Rdependency-scan \
// RUN:   -load-dependency-scan-cache -validate-prior-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   > %t/deps_stale.json 2> %t/remarks_stale.txt
// RUN: %FileCheck %s --check-prefix=REMARK-REUSE --implicit-check-not=invalidated < %t/remarks_stale.txt
// RUN: %validate-json %t/deps_stale.json &>/dev/null
// RUN: %FileCheck %s --check-prefix=DIRDEPS < %t/deps_stale.json
// RUN: %FileCheck %s --check-prefix=NO-ADDED < %t/deps_stale.json

// Reporting the directory invalidates the cached module, and Clang rebuilds the
// scanning module written by the first scan, picking up the header.
// RUN: %swift-scan-test -action scan_dependency -invalidated-path %t/include/sub -- %target-swift-frontend -emit-module -module-name Test \
// RUN:   -module-cache-path %t/module-cache -parse-stdlib \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -I %t/include -Rdependency-scan \
// RUN:   -load-dependency-scan-cache -validate-prior-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   > %t/deps_invalidated.json 2> %t/remarks_invalidated.txt
// RUN: %FileCheck %s --check-prefix=REMARK-INVALIDATED < %t/remarks_invalidated.txt
// RUN: %FileCheck %s --check-prefix=ADDED < %t/deps_invalidated.json

// REMARK-INITIAL: remark: Number of named Clang module queries: '1'
// REMARK-INITIAL: remark: Incremental module scan: Serializing module scanning dependency cache to:

// REMARK-REUSE: remark: Incremental module scan: Re-using serialized module scanning dependency cache from:
// REMARK-REUSE: remark: Number of named Clang module queries: '0'

// REMARK-INVALIDATED: remark: Incremental module scan: Re-using serialized module scanning dependency cache from:
// REMARK-INVALIDATED: remark: Incremental module scan: Dependency info for module 'UmbDir' invalidated due to a change in directory: '{{.*}}include{{[/\\]+}}sub'.
// REMARK-INVALIDATED: remark: Number of named Clang module queries: '1'

// DIRDEPS-LABEL: "modulePath": "{{.*}}UmbDir-{{.*}}.pcm",
// DIRDEPS:       "commandLine": [
// DIRDEPS:       ],
// DIRDEPS-NEXT:  "directoryDependencies": [
// DIRDEPS-NEXT:    "{{.*}}include{{[/\\]+}}sub"
// DIRDEPS-NEXT:  ]

// NO-ADDED-NOT: added.h

// ADDED-LABEL: "modulePath": "{{.*}}UmbDir-{{.*}}.pcm",
// ADDED:       "sourceFiles": [
// ADDED-DAG:   "{{.*}}include{{[/\\]+}}sub{{[/\\]+}}a.h"
// ADDED-DAG:   "{{.*}}include{{[/\\]+}}sub{{[/\\]+}}added.h"
// ADDED:       "directDependencies": [

//--- test.swift
import UmbDir

//--- include/module.modulemap
module UmbDir {
  umbrella "sub"
  module * { export * }
}

//--- include/sub/a.h
void a(void);
