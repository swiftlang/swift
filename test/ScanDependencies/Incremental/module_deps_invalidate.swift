// REQUIRES: OS=macosx
// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/Modules)
// RUN: %empty-directory(%t/ExtraCModules)
// RUN: %empty-directory(%t/BinModules)

// RUN: split-file %s %t

// Build Module D
// RUN: %target-swift-frontend -emit-module %t/D.swift -emit-module-path %t/Modules/D.swiftmodule/%target-swiftmodule-name -module-name D -enable-library-evolution -emit-module-interface-path %t/Modules/D.swiftmodule/%target-swiftinterface-name

// Build Module C
// RUN: %target-swift-frontend -emit-module %t/C.swift -emit-module-path %t/Modules/C.swiftmodule/%target-swiftmodule-name -module-name C -enable-library-evolution -emit-module-interface-path %t/Modules/C.swiftmodule/%target-swiftinterface-name -I %t/Modules -I %t/ExtraCModules

// Build Module B
// RUN: %target-swift-frontend -emit-module %t/B.swift -emit-module-path %t/Modules/B.swiftmodule/%target-swiftmodule-name -module-name B -enable-library-evolution -emit-module-interface-path %t/Modules/B.swiftmodule/%target-swiftinterface-name -I %t/Modules -I %t/ExtraCModules

// Build Module A
// RUN: %target-swift-frontend -emit-module %t/A.swift -emit-module-path %t/Modules/A.swiftmodule/%target-swiftmodule-name -module-name A -enable-library-evolution -emit-module-interface-path %t/Modules/A.swiftmodule/%target-swiftinterface-name -I %t/Modules -I %t/ExtraCModules

// Build binary-only Module E from its interface, so that E records the
// interface it was built from
// RUN: %target-swift-frontend -emit-module %t/E.swift -emit-module-path %t/E.swiftmodule -module-name E -enable-library-evolution -emit-module-interface-path %t/EInterface/E.swiftinterface
// RUN: %target-swift-frontend -compile-module-from-interface %t/EInterface/E.swiftinterface -o %t/BinModules/E.swiftmodule -module-name E

// Build binary-only Module F with a header import that includes another header
// RUN: %target-swift-frontend -emit-module %t/F.swift -emit-module-path %t/BinModules/F.swiftmodule -module-name F -import-objc-header %t/FHeaders/F-Bridging.h

// Initial Scan Client module
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_initial.json -I %t/Modules -I %t/BinModules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client

// Clean re-scan
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_clean_rescan.json -I %t/Modules -I %t/BinModules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/clean_incremental_scan_output.txt
// RUN: cat %t/clean_incremental_scan_output.txt | %FileCheck %s -check-prefix=CLEAN-INCREMENTAL-SCAN-CHECK

// Touch C and re-scan
// RUN: touch %t/Modules/C.swiftmodule/%target-swiftinterface-name
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_stale_C_rescan.json -I %t/Modules -I %t/BinModules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/stale_C_incremental_scan_output.txt
// RUN: cat %t/stale_C_incremental_scan_output.txt | %FileCheck %s -check-prefix=STALE-C-INCREMENTAL-SCAN-CHECK

// Clean re-scan
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_clean_rescan_2.json -I %t/Modules -I %t/BinModules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/clean_incremental_scan_output_2.txt
// RUN: cat %t/clean_incremental_scan_output_2.txt | %FileCheck %s -check-prefix=CLEAN-INCREMENTAL-SCAN-CHECK

// Replace a module dependency in A, ensure re-scan detects it
// RUN: echo "import X" > %t/A.swift
// RUN: %target-swift-frontend -emit-module %t/A.swift -emit-module-path %t/Modules/A.swiftmodule/%target-swiftmodule-name -module-name A -enable-library-evolution -emit-module-interface-path %t/Modules/A.swiftmodule/%target-swiftinterface-name -I %t/Modules -I %t/ExtraCModules -I %S/../Inputs/CHeaders

// Re-scan to ensure A gets scanned again and the new dependency is picked up.
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_new_A_rescan.json -I %t/Modules -I %t/BinModules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/new_A_incremental_scan_output.txt
// RUN: cat %t/new_A_incremental_scan_output.txt | %FileCheck %s -check-prefix=NEW-A-INCREMENTAL-SCAN-CHECK
// RUN: %validate-json %t/deps_new_A_rescan.json | %FileCheck %s --check-prefix=NEW-A-DEPS-CHECK

// Touch a header in Clang module Z and re-scan
// RUN: touch %t/ExtraCModules/Z.h
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_stale_Z_rescan.json -I %t/Modules -I %t/BinModules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/stale_Z_incremental_scan_output.txt
// RUN: cat %t/stale_Z_incremental_scan_output.txt | %FileCheck %s -check-prefix=STALE-Z-INCREMENTAL-SCAN-CHECK

// Change a header in Clang module Z while keeping its modification time older
// than the prior scan, and re-scan
// RUN: cp %t/Z_v2.h %t/ExtraCModules/Z.h
// RUN: touch -t 201401240005 %t/ExtraCModules/Z.h
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_edited_Z.json -I %t/Modules -I %t/BinModules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/edited_Z_incremental_scan_output.txt
// RUN: cat %t/edited_Z_incremental_scan_output.txt | %FileCheck %s -check-prefix=EDITED-Z-INCREMENTAL-SCAN-CHECK

// Change the interface of A while keeping its modification time older than the
// prior scan, and re-scan
// RUN: %target-swift-frontend -emit-module %t/A_v2.swift -emit-module-path %t/Modules/A.swiftmodule/%target-swiftmodule-name -module-name A -enable-library-evolution -emit-module-interface-path %t/Modules/A.swiftmodule/%target-swiftinterface-name -I %t/Modules -I %t/ExtraCModules -I %S/../Inputs/CHeaders
// RUN: touch -t 201401240005 %t/Modules/A.swiftmodule/%target-swiftinterface-name
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_edited_A.json -I %t/Modules -I %t/BinModules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/edited_A_incremental_scan_output.txt
// RUN: cat %t/edited_A_incremental_scan_output.txt | %FileCheck %s -check-prefix=EDITED-A-INCREMENTAL-SCAN-CHECK

// Change the interface binary module E was built from while keeping its
// modification time older than the prior scan, and re-scan
// RUN: %target-swift-frontend -emit-module %t/E_v2.swift -emit-module-path %t/E.swiftmodule -module-name E -enable-library-evolution -emit-module-interface-path %t/EInterface/E.swiftinterface
// RUN: touch -t 201401240005 %t/EInterface/E.swiftinterface
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_edited_E.json -I %t/Modules -I %t/BinModules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/edited_E_incremental_scan_output.txt
// RUN: cat %t/edited_E_incremental_scan_output.txt | %FileCheck %s -check-prefix=EDITED-E-INCREMENTAL-SCAN-CHECK

// Change a header included by binary module F's header import while keeping
// its modification time older than the prior scan, and re-scan
// RUN: cp %t/FInner_v2.h %t/FHeaders/FInner.h
// RUN: touch -t 201401240005 %t/FHeaders/FInner.h
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/Client.swift -o %t/deps_edited_F.json -I %t/Modules -I %t/BinModules -I %t/ExtraCModules -I %S/../Inputs/CHeaders -module-name Client -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache &> %t/edited_F_incremental_scan_output.txt
// RUN: cat %t/edited_F_incremental_scan_output.txt | %FileCheck %s -check-prefix=EDITED-F-INCREMENTAL-SCAN-CHECK

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

// EDITED-Z-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// EDITED-Z-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Dependency info for module 'Z' invalidated due to a modified input since last scan: {{.*}}Z.h
// EDITED-Z-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache

// EDITED-A-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// EDITED-A-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Dependency info for module 'A' invalidated due to a modified input since last scan: {{.*}}A.swiftmodule{{.*}}swiftinterface
// EDITED-A-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache

// EDITED-E-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// EDITED-E-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Dependency info for module 'E' invalidated due to a modified input since last scan: {{.*}}EInterface{{/|\\}}E.swiftinterface
// EDITED-E-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache

//--- ExtraCModules/Z.h
void funcZ(void);

//--- Z_v2.h
void funcZ(void);
void funcZ2(void);

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

//--- A_v2.swift
import X
public func bar() {}

// EDITED-F-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// EDITED-F-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Dependency info for module 'F' invalidated due to a modified input since last scan: {{.*}}FInner.h
// EDITED-F-INCREMENTAL-SCAN-CHECK: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache

//--- FHeaders/F-Bridging.h
#include "FInner.h"

//--- FHeaders/FInner.h
void funcF(void);

//--- FInner_v2.h
void funcF(void);
void funcF2(void);

//--- F.swift
public func f() { funcF() }

//--- E.swift
public func baz() {}

//--- E_v2.swift
public func baz() {}
public func qux() {}

//--- Client.swift
import A
import E
import F
import Z
