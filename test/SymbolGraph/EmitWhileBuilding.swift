// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name EmitWhileBuilding -emit-module -emit-module-path %t/EmitWhileBuilding.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding.symbols.json

// also try without the trailing slash on `-emit-symbol-graph-dir` and make sure it works

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name EmitWhileBuilding -emit-module -emit-module-path %t/EmitWhileBuilding.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding.symbols.json

// also try while forcing the use of supplementary file maps to make sure the symbol graph path gets
// added to the file map for the inner frontend call

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name EmitWhileBuilding -emit-module -emit-module-path %t/EmitWhileBuilding.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t -driver-filelist-threshold=0 -O -whole-module-optimization
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding.symbols.json

// also try with an up-to-date incremental build to make sure that adding the symbol graph flags
// can get them to be generated

// RUN: %empty-directory(%t)
// RUN: cp %s %t
// RUN: cd %t && %target-build-swift %t/EmitWhileBuilding.swift -module-name EmitWhileBuilding -c -emit-module -emit-module-path %t/EmitWhileBuilding.swiftmodule -emit-dependencies -incremental -output-file-map=%S/Inputs/EmitWhileBuilding.output.json -working-directory %t -v -driver-show-incremental
// RUN: cd %t && %target-build-swift %t/EmitWhileBuilding.swift -module-name EmitWhileBuilding -c -emit-module -emit-module-path %t/EmitWhileBuilding.swiftmodule -emit-dependencies -incremental -output-file-map=%S/Inputs/EmitWhileBuilding.output.json -working-directory %t -v -driver-show-incremental -emit-symbol-graph -emit-symbol-graph-dir %t
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding.symbols.json

/// Does a foo.
public func foo() {}

// CHECK: "precise":"s:17EmitWhileBuilding3fooyyF"
