// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: split-file %s %t

// Run a dependency scan on a source file that imports both constituents of a cross-import overlay to ensure the cross-import overlay dependency is registered
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/C.swift -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -I %S/Inputs/CHeaders/ExtraCModules -module-name FineGrainedCrossImportTestModule -enable-cross-import-overlays
// Check the contents of the JSON output
// RUN: %validate-json %t/deps.json | %FileCheck %s --check-prefix CHECK-TOGETHER

// Run a dependency scan on two source files that separately import constituents of a cross-import overlay and ensure that the cross-import overlay dependency is *not* registered
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/A.swift %t/B.swift -o %t/deps_disjoint.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -I %S/Inputs/CHeaders/ExtraCModules -module-name FineGrainedCrossImportTestModule -enable-cross-import-overlays
// Check the contents of the JSON output
// RUN: %validate-json %t/deps_disjoint.json | %FileCheck %s --check-prefix CHECK-DISJOINT

//--- A.swift
import E

//--- B.swift
import SubE

//--- C.swift
import E
import SubE

// CHECK-TOGETHER: "swift": "_cross_import_E"
// CHECK-DISJOINT-NOT: "swift": "_cross_import_E"
