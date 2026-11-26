// REQUIRES: legacy_swift_driver

// RUN: not %swiftc_driver -import-prescan %s 2>&1 | %FileCheck %s --check-prefix=IMPORT-PRESCAN
// RUN: not %swiftc_driver -scan-dependencies -load-dependency-scan-cache %s 2>&1 \
// RUN:   | %FileCheck %s --check-prefix=LOAD-CACHE

// IMPORT-PRESCAN: error: '-import-prescan' requires '-scan-dependencies'
// IMPORT-PRESCAN-NOT: error: '-import-prescan' requires '-scan-dependencies'

// LOAD-CACHE: error: '-load-dependency-scan-cache' requires '-dependency-scan-cache-path'
