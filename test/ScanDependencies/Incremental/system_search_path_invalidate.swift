// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/Frameworks)

// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %s -o %t/deps.json -I %t/ExtraCModules -I %S/../Inputs/CHeaders -F %t/Frameworks &> %t/first_scan_output.txt
// RUN: cat %t/first_scan_output.txt | %FileCheck %s

// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %s -o %t/deps.json -I %t/ExtraCModules -I %S/../Inputs/CHeaders -F %t/Frameworks &> %t/second_scan_output.txt
// RUN: cat %t/second_scan_output.txt | %FileCheck %s -check-prefix=INCREMENTAL-CHECK

// Change an '-F' to a '-Fsystem' and ensure that the serialized cache does not get reused
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %s -o %t/deps.json -I %t/ExtraCModules -I %S/../Inputs/CHeaders -Fsystem %t/Frameworks &> %t/second_system_scan_output.txt
// RUN: cat %t/second_system_scan_output.txt | %FileCheck %s

// CHECK: remark: Incremental module scan: Failed to load module scanning dependency cache from:
// INCREMENTAL-CHECK-NOT: remark: Incremental module scan: Failed to load module scanning dependency cache from:

