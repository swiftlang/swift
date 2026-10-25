// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache

/// Change X.h without updating its modification time, so the prior scan's
/// entry for X is reused, then touch Y.h so that rescanning Y reports a new
/// variant of X.
// RUN: echo "void funcX2(void);" >> %t/X.h
// RUN: touch -t 201401240005 %t/X.h
// RUN: touch %t/Y.h

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | \
// RUN:   %FileCheck %s --implicit-check-not=error: --implicit-check-not=warning:

// CHECK: remark: Incremental module scan: Dependency info for module 'Y' invalidated due to a modified input since last scan: {{.*}}Y.h
// CHECK: remark: Incremental module scan: Dependency info for module 'X' invalidated due to a new variant discovered by this scan, re-scanning all modules.
// CHECK: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache

/// The result must match a scan that does not use the prior scan cache.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps_scratch.json -I %t -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib
// RUN: diff %t/deps.json %t/deps_scratch.json

//--- Test.swift
import Y

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

//--- W.h
void funcW(void);

//--- X.h
#include "W.h"
void funcX(void);

//--- Y.h
#include "X.h"
void funcY(void);
