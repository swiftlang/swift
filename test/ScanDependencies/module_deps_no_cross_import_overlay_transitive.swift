// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/clang-module-cache)
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -I %S/Inputs/CHeaders/ExtraCModules -module-name CrossImportTestModule -enable-cross-import-overlays
// Check the contents of the JSON output
// RUN: %validate-json %t/deps.json | %FileCheck %s

// Ensure that round-trip serialization does not affect result
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -test-dependency-scan-cache-serialization -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -I %S/Inputs/CHeaders/ExtraCModules -module-name CrossImportTestModule -enable-cross-import-overlays
// RUN: %validate-json %t/deps.json | %FileCheck %s

// Ensure that if one of the two components of a cross-import overlay is a transitive (non-exported) Swift dependency, then no overlay is required to be brought in.
import EWrapperNonExported
import SubEWrapper

// CHECK-NOT: "swift": "_cross_import_E"
