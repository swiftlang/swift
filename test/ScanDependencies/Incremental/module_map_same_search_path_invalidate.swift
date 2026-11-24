// REQUIRES: objc_interop

// Check a newly introduced module map that shadows an existing one
// in the same search path is invalidated.

// Test.swift imports ClangMod and the only search path is Include.
// In the first build ClangMod is defined by Include/ClangMod/module.modulemap. 
// In the second build, ClangMod should resolve Include/module.modulemap instead.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/test.swift -o %t/deps_initial.json -module-name Test -I %t/Include -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// RUN: %validate-json %t/deps_initial.json | %FileCheck %s -check-prefix=INITIAL

// Add Include/module.modulemap, which also defines ClangMod.
// RUN: cp %t/Variants/module.modulemap %t/Include/module.modulemap
// RUN: cp %t/Variants/ClangModNew.h %t/Include/ClangModNew.h
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/test.swift -o %t/deps_rescan.json -module-name Test -I %t/Include -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -validate-prior-dependency-scan-cache &> %t/rescan_output.txt
// RUN: cat %t/rescan_output.txt | %FileCheck %s -check-prefix=RESCAN-OUTPUT
// RUN: %validate-json %t/deps_rescan.json | %FileCheck %s -check-prefix=RESCAN

// Scan without a dependency scan cache.
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -module-cache-path %t/module-cache %t/test.swift -o %t/deps_clean.json -module-name Test -I %t/Include -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// RUN: diff %t/deps_rescan.json %t/deps_clean.json

// INITIAL: "moduleMapPath": "{{.*}}Include{{/|\\\\}}ClangMod{{/|\\\\}}module.modulemap"

// RESCAN-OUTPUT: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// RESCAN-OUTPUT: remark: Incremental module scan: Dependency info for module 'ClangMod' invalidated because the module was previously defined in '{{.*}}Include{{[/\\]}}ClangMod{{[/\\]}}module.modulemap' but is now found in '{{.*}}Include{{[/\\]}}module.modulemap'.

// RESCAN: "moduleMapPath": "{{.*}}Include{{/|\\\\}}module.modulemap"

//--- test.swift
import ClangMod

//--- Include/ClangMod/module.modulemap
module ClangMod {
  header "ClangMod.h"
  export *
}

//--- Include/ClangMod/ClangMod.h
void clangmod(void);

//--- Variants/module.modulemap
module ClangMod {
  header "ClangModNew.h"
  export *
}

//--- Variants/ClangModNew.h
void clangmod_new(void);
