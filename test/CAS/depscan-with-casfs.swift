// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir %t/smod-dir

// RUN: %target-swift-frontend -emit-module -module-name SMod \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-cache-path %t/clang-module-cache %t/smod.swift -o %t/smod-dir/SMod.swiftmodule

// Normal scanning, make sure everything is setup correctly.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -I %t/smod-dir -I %t/omod-dir \
// RUN:   %t/main.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas 2>&1 | %FileCheck %s --check-prefix=SCAN_NOERR -allow-empty
// SCAN_NOERR-NOT: error:

// RUN: %swift-scan-test -action=create_casfs -cas-path %t/cas > %t/empty.id

// Scanning with empty CASFS, make sure the swift file is not visible.
// RUN: not %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -I %t/smod-dir -I %t/omod-dir \
// RUN:   -scanner-cas-fs @%t/empty.id -cas-fs-escape %test-resource-dir \
// RUN:   %t/main.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas 2> %t/empty-fail.txt
// RUN: %FileCheck %s --check-prefix=SCAN_EMPTY --input-file %t/empty-fail.txt
// SCAN_EMPTY: error: error opening input file '{{.*}}/main.swift' (No such file or directory)

// RUN: %swift-scan-test -action=create_casfs -cas-path %t/cas %t/main.swift > %t/main-only.id

// Scanning with CASFS without including the modules, make sure modules are not found.
// RUN: not %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -I %t/smod-dir -I %t/omod-dir \
// RUN:   -scanner-cas-fs @%t/main-only.id -cas-fs-escape %test-resource-dir \
// RUN:   %t/main.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas 2> %t/fail.txt
// RUN: %FileCheck %s --check-prefix=SCAN_BOTHFAIL --input-file %t/fail.txt
// SCAN_BOTHFAIL: error: unable to resolve module dependency: 'SMod'
// SCAN_BOTHFAIL: error: unable to resolve module dependency: 'OMod'

// Scanning with CASFS without including the modules but escaping their paths, make sure modules are found.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -I %t/smod-dir -I %t/omod-dir \
// RUN:   -scanner-cas-fs @%t/main-only.id -cas-fs-escape %test-resource-dir -cas-fs-escape %t/smod-dir -cas-fs-escape %t/omod-dir \
// RUN:   %t/main.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas 2>&1 | %FileCheck %s --check-prefix=SCAN_NOERR -allow-empty

// RUN: %swift-scan-test -action=create_casfs -cas-path %t/cas @%t/main-only.id %t/smod-dir %t/omod-dir > %t/allmods.id

// Scanning with CASFS including the modules, make sure modules are found.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -I %t/smod-dir -I %t/omod-dir \
// RUN:   -scanner-cas-fs @%t/allmods.id -cas-fs-escape %test-resource-dir \
// RUN:   %t/main.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas 2>&1 | %FileCheck %s --check-prefix=SCAN_NOERR -allow-empty

// RUN: %swift-scan-test -action=create_casfs -cas-path %t/cas %t/second.swift > %t/second-only.id

// Scan `second.swift` (which creates the scanning service) and `main.swift` (which reuses it)
// within the same `swift-scan-test` process, each with its own CASFS root that only contains what that invocation needs.
// RUN: %swift-scan-test -action scan_dependency -cas-path %t/cas \
// RUN:   -main-executable-path %swift_frontend_plain \
// RUN:   -o %t/deps1.json -o %t/deps2.json -- \
// RUN:   %target-swift-frontend -scan-dependencies -module-name Test1 -O \
// RUN:     -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:     -scanner-cas-fs @%t/second-only.id -cas-fs-escape %test-resource-dir \
// RUN:     %t/second.swift -o %t/deps1.json -cache-compile-job -cas-path %t/cas \
// RUN:   -- \
// RUN:   %target-swift-frontend -scan-dependencies -module-name Test2 -O \
// RUN:     -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -I %t/smod-dir -I %t/omod-dir \
// RUN:     -scanner-cas-fs @%t/allmods.id -cas-fs-escape %test-resource-dir \
// RUN:     %t/main.swift -o %t/deps2.json -cache-compile-job -cas-path %t/cas 2>&1 | %FileCheck %s --check-prefix=SCAN_NOERR -allow-empty

//--- main.swift
import SMod
import OMod

smod_func()
omod_func()

//--- smod.swift
public func smod_func() {}

//--- omod-dir/module.modulemap
module OMod {
  header "omod.h"
}

//--- omod-dir/omod.h
void omod_func(void);

// Make sure dep-scan lexing is happening, in which case #error directives are skipped.
#error depscan lexing not working

//--- second.swift
