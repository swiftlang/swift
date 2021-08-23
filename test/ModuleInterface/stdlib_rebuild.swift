// RUN: %empty-directory(%t.mcps)

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs/stdlib_rebuild -resource-dir %S/Inputs/stdlib_rebuild -module-cache-path %t.mcps/rebuild-remarks-off) -typecheck %s 2>&1 | %FileCheck -check-prefixes SLOW-DIAG,ALL %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs/stdlib_rebuild -resource-dir %S/Inputs/stdlib_rebuild -module-cache-path %t.mcps/rebuild-remarks-off) -typecheck %s 2>&1 | %FileCheck -check-prefixes ALL --allow-empty %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs/stdlib_rebuild -resource-dir %S/Inputs/stdlib_rebuild -module-cache-path %t.mcps/rebuild-remarks-off) -D OTHER_IMPORT -typecheck %s 2>&1 | %FileCheck -check-prefixes ALL --allow-empty %s

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs/stdlib_rebuild -resource-dir %S/Inputs/stdlib_rebuild -module-cache-path %t.mcps/rebuild-remarks-on) -Rmodule-interface-rebuild -typecheck %s 2>&1 | %FileCheck -check-prefixes NORMAL-DIAG,ALL %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs/stdlib_rebuild -resource-dir %S/Inputs/stdlib_rebuild -module-cache-path %t.mcps/rebuild-remarks-on) -Rmodule-interface-rebuild -typecheck %s 2>&1 | %FileCheck -check-prefixes ALL --allow-empty %s

// Our test directory only contains interfaces for x86_64-apple-macos.
// REQUIRES: CPU=x86_64
// REQUIRES: OS=macosx

#if OTHER_IMPORT
import OtherModule
#endif

func fn(_: Int) {}

// SLOW-DIAG: remark: did not find a prebuilt standard library for target '{{.*}}' compatible with this Swift compiler; building it may take a few minutes, but it should only happen once for this combination of compiler and target

// NORMAL-DIAG-DAG: remark: rebuilding module 'Swift' from interface '{{.*}}'
// NORMAL-DIAG-DAG: remark: rebuilding module '_Concurrency' from interface '{{.*}}'

// Used even when one of the above ones is also used, since the diagnostics should only be emitted once
// ALL-NOT: remark: did not find a prebuilt standard library
// ALL-NOT: remark: rebuilding module '{{Swift|_Concurrency}}' from interface '{{.*}}'
