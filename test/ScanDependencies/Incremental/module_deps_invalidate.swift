// REQUIRES: OS=macosx
// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/Modules)
// RUN: %empty-directory(%t/ExtraCModules)

// RUN: split-file %s %t

// Build Module D
// RUN: %target-swift-frontend -emit-module %t/D.swift -emit-module-path %t/Modules/D.swiftmodule/%target-swiftmodule-name -module-name D -enable-library-evolution -emit-module-interface-path %t/Modules/D.swiftmodule/%target-swiftinterface-name

// Build Module C
// RUN: %target-swift-frontend -emit-module %t/C.swift -emit-module-path %t/Modules/C.swiftmodule/%target-swiftmodule-name -module-name C -enable-library-evolution -emit-module-interface-path %t/Modules/C.swiftmodule/%target-swiftinterface-name -I %t/Modules -I %t/ExtraCModules

// Build Module B
// RUN: %target-swift-frontend -emit-module %t/B.swift -emit-module-path %t/Modules/B.swiftmodule/%target-swiftmodule-name -module-name B -enable-library-evolution -emit-module-interface-path %t/Modules/B.swiftmodule/%target-swiftinterface-name -I %t/Modules -I %t/ExtraCModules

// Build Module A
// RUN: %target-swift-frontend -emit-module %t/A.swift -emit-module-path %t/Modules/A.swiftmodule/%target-swiftmodule-name -module-name A -enable-library-evolution -emit-module-interface-path %t/Modules/A.swiftmodule/%target-swiftinterface-name -I %t/Modules -I %t/ExtraCModules

// Initial Scan Client module
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_initial.json -I %t/Modules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client

// Clean re-scan
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_clean_rescan.json -I %t/Modules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/clean_incremental_scan_output.txt
// RUN: cat %t/clean_incremental_scan_output.txt | %FileCheck %s -check-prefix=CLEAN-INCREMENTAL-SCAN-CHECK

// Touch C and re-scan
// RUN: touch %t/Modules/C.swiftmodule/%target-swiftinterface-name
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_stale_C_rescan.json -I %t/Modules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/stale_C_incremental_scan_output.txt
// RUN: cat %t/stale_C_incremental_scan_output.txt | %FileCheck %s -check-prefix=STALE-C-INCREMENTAL-SCAN-CHECK

// Clean re-scan
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_clean_rescan_2.json -I %t/Modules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/clean_incremental_scan_output_2.txt
// RUN: cat %t/clean_incremental_scan_output_2.txt | %FileCheck %s -check-prefix=CLEAN-INCREMENTAL-SCAN-CHECK

// Replace a module dependency in A, ensure re-scan detects it
// RUN: echo "import X" > %t/A.swift
// RUN: %target-swift-frontend -emit-module %t/A.swift -emit-module-path %t/Modules/A.swiftmodule/%target-swiftmodule-name -module-name A -enable-library-evolution -emit-module-interface-path %t/Modules/A.swiftmodule/%target-swiftinterface-name -I %t/Modules -I %t/ExtraCModules -I %S/../Inputs/CHeaders

// Re-scan to ensure A gets scanned again and the new dependency is picked up.
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_new_A_rescan.json -I %t/Modules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/new_A_incremental_scan_output.txt
// RUN: cat %t/new_A_incremental_scan_output.txt | %FileCheck %s -check-prefix=NEW-A-INCREMENTAL-SCAN-CHECK
// RUN: %validate-json %t/deps_new_A_rescan.json | %FileCheck %s --check-prefix=NEW-A-DEPS-CHECK

// Touch a header in Clang module Z and re-scan
// RUN: touch %t/ExtraCModules/Z.h
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_stale_Z_rescan.json -I %t/Modules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/stale_Z_incremental_scan_output.txt
// RUN: cat %t/stale_Z_incremental_scan_output.txt | %FileCheck %s -check-prefix=STALE-Z-INCREMENTAL-SCAN-CHECK

// CLEAN-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// CLEAN-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache
// CLEAN-INCREMENTAL-SCAN-CHECK-NOT: remark: Incremental module scan: Dependency info for module '{{.*}}' invalidated due to

// STALE-C-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// STALE-C-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Dependency info for module 'C' invalidated due to a modified input since last scan: {{.*}}C.swiftmodule{{.*}}swiftinterface
// STALE-C-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Dependency info for module 'B' invalidated due to an out-of-date dependency.
// STALE-C-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Dependency info for module 'A' invalidated due to an out-of-date dependency.
// STALE-C-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache
// STALE-C-INCREMENTAL-SCAN-CHECK-NOT: remark: Incremental module scan: Dependency info for module 'D' invalidated

// NEW-A-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// NEW-A-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Dependency info for module 'A' invalidated due to a modified input since last scan: {{.*}}A.swiftmodule{{.*}}swiftinterface
// NEW-A-DEPS-CHECK: "clang": "X"

// STALE-Z-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// STALE-Z-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Dependency info for module 'Z' invalidated due to a modified input since last scan: {{.*}}Z.h
// STALE-Z-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache

//--- ExtraCModules/Z.h
void funcZ(void);

//--- ExtraCModules/module.modulemap
module Z {
  header "Z.h"
  export *
}

//--- A.swift
import B

//--- B.swift
import C

//--- C.swift
import D

//--- D.swift
public func foo() {}

//--- Client.swift
import A
import Z
