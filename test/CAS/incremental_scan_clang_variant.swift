// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: cp %t/Test_import.swift %t/Test.swift

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache

/// Nothing changed: every module is reused.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | \
// RUN:   %FileCheck %s --check-prefix=CLEAN --implicit-check-not=error: --implicit-check-not=warning: --implicit-check-not=invalidated

// CLEAN: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// CLEAN: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache

/// Change X.h and move its modification time back to before the prior scan.
/// X and everything that depends on it must be invalidated.
// RUN: cp %t/X_v2.h %t/X.h
// RUN: touch -t 201401240005 %t/X.h

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | \
// RUN:   %FileCheck %s --check-prefix=EDIT-X --implicit-check-not=error: --implicit-check-not=warning: --implicit-check-not=invalidated

// EDIT-X: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// EDIT-X: remark: Incremental module scan: Dependency info for module 'X' invalidated due to a modified input since last scan: '{{.*}}X.h'.
// EDIT-X: remark: Incremental module scan: Dependency info for module 'Y' invalidated due to an out-of-date dependency.
// EDIT-X: remark: Incremental module scan: Dependency info for module 'Test' invalidated due to an out-of-date dependency.
// EDIT-X: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache

/// The cache keys must match a scan that does not use the prior scan cache.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps_scratch.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:X moduleCacheKey > %t/X.key
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps_scratch.json clang:X moduleCacheKey > %t/X_scratch.key
// RUN: %FileCheck %s --check-prefix=KEY < %t/X_scratch.key
// RUN: diff %t/X.key %t/X_scratch.key
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:Y moduleCacheKey > %t/Y.key
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps_scratch.json clang:Y moduleCacheKey > %t/Y_scratch.key
// RUN: %FileCheck %s --check-prefix=KEY < %t/Y_scratch.key
// RUN: diff %t/Y.key %t/Y_scratch.key

/// Change the size of X.h but restore the modification time the prior scan
/// recorded for it, and touch Y.h. X must be invalidated before Y is
/// re-scanned, so that re-scanning Y does not discover a second variant of X.
// RUN: %empty-directory(%t/saved)
// RUN: cp -p %t/X.h %t/saved/X.h
// RUN: cp %t/X_v3.h %t/X.h
// RUN: touch -r %t/saved/X.h %t/X.h
// RUN: touch %t/Y.h

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | \
// RUN:   %FileCheck %s --check-prefix=EDIT-X --implicit-check-not=error: --implicit-check-not=warning: --implicit-check-not=invalidated

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps_scratch.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:X moduleCacheKey > %t/X.key
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps_scratch.json clang:X moduleCacheKey > %t/X_scratch.key
// RUN: %FileCheck %s --check-prefix=KEY < %t/X_scratch.key
// RUN: diff %t/X.key %t/X_scratch.key

/// Change X.h without changing its size, with a different modification time
/// that is still before the prior scan.
// RUN: cp %t/X_v4.h %t/X.h
// RUN: touch -t 201401240006 %t/X.h

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | \
// RUN:   %FileCheck %s --check-prefix=EDIT-X --implicit-check-not=error: --implicit-check-not=warning: --implicit-check-not=invalidated

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps_scratch.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:X moduleCacheKey > %t/X.key
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps_scratch.json clang:X moduleCacheKey > %t/X_scratch.key
// RUN: %FileCheck %s --check-prefix=KEY < %t/X_scratch.key
// RUN: diff %t/X.key %t/X_scratch.key

/// Stop importing Y, so that X and Y are no longer reachable from the main
/// module, then change X.h while they are unreachable. Once Y is imported
/// again, X must reflect the change.
// RUN: cp %t/Test_noimport.swift %t/Test.swift
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache
// RUN: cp %t/X_v5.h %t/X.h
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache
// RUN: cp %t/Test_import.swift %t/Test.swift
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | \
// RUN:   %FileCheck %s --check-prefix=REACHABLE --implicit-check-not=error: --implicit-check-not=warning:

// REACHABLE: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// REACHABLE: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:X directDependencies | %FileCheck %s --check-prefix=X-DEPS
// X-DEPS-DAG: "clang": "W"
// X-DEPS-DAG: "clang": "Z"

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps_scratch.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:X moduleCacheKey > %t/X.key
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps_scratch.json clang:X moduleCacheKey > %t/X_scratch.key
// RUN: %FileCheck %s --check-prefix=KEY < %t/X_scratch.key
// RUN: diff %t/X.key %t/X_scratch.key

// KEY: llvmcas://

//--- Test_import.swift
import Y

//--- Test_noimport.swift

//--- module.modulemap
module W {
  header "W.h"
  export *
}
module X {
  header "X.h"
  export *
}
module Y {
  header "Y.h"
  export *
}
module Z {
  header "Z.h"
  export *
}

//--- W.h
void funcW(void);

//--- X.h
#include "W.h"
void funcX(void);

//--- Y.h
#include "X.h"
void funcY(void);

//--- Z.h
void funcZ(void);

//--- X_v2.h
#include "W.h"
void funcX(void);
void funcX2(void);

//--- X_v3.h
#include "W.h"
void funcX(void);
void funcX2(void);
void funcX3(void);

//--- X_v4.h
#include "W.h"
void funcX(void);
void funcX2(void);
void funcX4(void);

//--- X_v5.h
#include "W.h"
#include "Z.h"
void funcX(void);
