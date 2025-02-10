// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/../ScanDependencies/Inputs/Swift -cache-compile-job -cas-path %t/cas -no-clang-include-tree -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/clean_scan_output.txt
// RUN: cat %t/clean_scan_output.txt | %FileCheck %s

// CHECK: remark: Incremental module scan: Re-using serialized module scanning dependency cache disabled in Caching build
import E
