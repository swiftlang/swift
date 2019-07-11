// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t/UsesImportedEnums-a.swiftmodule -parse-as-library %S/Inputs/use_imported_enums.swift -module-name UsesImportedEnums
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -merge-modules -module-name UsesImportedEnums -emit-module -o %t/UsesImportedEnums.swiftmodule %t/UsesImportedEnums-a.swiftmodule

// REQUIRES: objc_interop
