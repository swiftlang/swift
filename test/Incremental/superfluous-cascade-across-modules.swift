// =============================================================================
// Without private dependencies
// =============================================================================

// First, build a submodule

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/superfluous-cascade-across-modules/*.swift %t
// RUN: cp %S/Inputs/superfluous-cascade-across-modules/*.json %t

// RUN: %target-build-swift %S/Inputs/superfluous-cascade-across-modules/submodule/lib-before.swift -emit-module -emit-library -module-name Lib -module-link-name Lib -emit-module-path %t/Lib.swiftmodule -o %t/%target-library-name(Lib)

// Build the main executable that depends on the submodule we just built

// RUN: cd %t && %swiftc_driver -emit-module -enable-batch-mode -j2 -incremental -driver-show-incremental -I %t -L %t -lLib -module-name main \
// RUN:   -output-file-map ofm.json \
// RUN:   main.swift \
// RUN:   doesNotUseLib.swift \
// RUN:   usesLib.swift \
// RUN:   usesLibTransitively.swift >&output1

// Rebuild the submodule

// RUN: %target-build-swift %S/Inputs/superfluous-cascade-across-modules/submodule/lib-after.swift -emit-module -emit-library -module-name Lib -module-link-name Lib -emit-module-path %t/Lib.swiftmodule -o %t/%target-library-name(Lib)

// Rebuild the main executable

// RUN: cd %t && %swiftc_driver -emit-module -enable-batch-mode -j2 -incremental -driver-show-incremental -I %t -L %t -lLib -module-name main \
// RUN:   -output-file-map ofm.json \
// RUN:   main.swift \
// RUN:   doesNotUseLib.swift \
// RUN:   usesLib.swift \
// RUN:   usesLibTransitively.swift >&output2

// RUN: %FileCheck -check-prefix=CHECK-STATUS-QUO-RECOMPILED %s < %t/output2

// CHECK-STATUS-QUO-RECOMPILED-DAG: Queuing because of external dependencies: {compile: main
// CHECK-STATUS-QUO-RECOMPILED-DAG: Queuing because of external dependencies: {compile: usesLib
// CHECK-STATUS-QUO-RECOMPILED-DAG: Queuing because of external dependencies: {compile: doesNotUseLib
