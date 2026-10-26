// REQUIRES: objc_interop

// Verify that a clang module that is only reached through a header include, is not invalidated.

// Test.swift imports nothing. Its bridging header includes Sub/clangmod.h,
// which belongs to ClangMod. A lookup of ClangMod by name does not find it.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// RUN: split-file %s %t

// Initial scan: ClangMod resolves to Include/Sub/module.modulemap through the
// bridging header's include.
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/test.swift -o %t/deps_initial.json -module-name Test -I %t/Include -import-objc-header %t/bridge.h -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// RUN: %validate-json %t/deps_initial.json | %FileCheck %s -check-prefix=DEPS

// Rescan with no changes: the module map lookup does not find ClangMod.
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/test.swift -o %t/deps_noop.json -module-name Test -I %t/Include -import-objc-header %t/bridge.h -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -validate-prior-dependency-scan-cache &> %t/noop_output.txt
// RUN: cat %t/noop_output.txt | %FileCheck %s -check-prefix=NOOP-OUTPUT
// RUN: %validate-json %t/deps_noop.json | %FileCheck %s -check-prefix=DEPS

// DEPS: "moduleMapPath": "{{.*}}Include{{/|\\\\}}Sub{{/|\\\\}}module.modulemap"

// NOOP-OUTPUT: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// NOOP-OUTPUT: remark: Incremental module scan: Module map lookup did not find module 'ClangMod'.
// NOOP-OUTPUT-NOT: invalidated

//--- test.swift

//--- bridge.h
#include "Sub/clangmod.h"

//--- Include/Sub/clangmod.h
void clangmod(void);

//--- Include/Sub/module.modulemap
module ClangMod {
  header "clangmod.h"
  export *
}
