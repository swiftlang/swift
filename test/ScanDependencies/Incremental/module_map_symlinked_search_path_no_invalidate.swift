// REQUIRES: objc_interop

// Check that the module map recorded by the initial scan and the one the
// rescan's lookup finds resolve to the same canonical path, allowing the result to be reused. 

// Test.swift imports ClangMod using the search path, Link, which is a symlink
// to Frameworks. The resolved module map dependency should be in Frameworks.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// RUN: split-file %s %t
// RUN: ln -s %t/Frameworks %t/Link

// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/test.swift -o %t/deps_initial.json -module-name Test -F %t/Link -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// RUN: %validate-json %t/deps_initial.json | %FileCheck %s -check-prefix=DEPS

// Rescan with no changes: ClangMod is not invalidated.
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/test.swift -o %t/deps_noop.json -module-name Test -F %t/Link -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -validate-prior-dependency-scan-cache &> %t/noop_output.txt
// RUN: cat %t/noop_output.txt | %FileCheck %s -check-prefix=NOOP-OUTPUT
// RUN: %validate-json %t/deps_noop.json | %FileCheck %s -check-prefix=DEPS

// The source files keep the symlinked spelling; the module map path does not.
// DEPS: "{{.*}}Link{{/|\\\\}}ClangMod.framework{{/|\\\\}}Modules{{/|\\\\}}module.modulemap"
// DEPS: "moduleMapPath": "{{.*}}Frameworks{{/|\\\\}}ClangMod.framework{{/|\\\\}}Modules{{/|\\\\}}module.modulemap"

// NOOP-OUTPUT: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// NOOP-OUTPUT-NOT: invalidated

//--- test.swift
import ClangMod

//--- Frameworks/ClangMod.framework/Headers/ClangMod.h
void clangmod(void);

//--- Frameworks/ClangMod.framework/Modules/module.modulemap
framework module ClangMod {
  header "ClangMod.h"
  export *
}
