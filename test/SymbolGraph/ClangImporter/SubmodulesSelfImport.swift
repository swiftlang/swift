// RUN: %empty-directory(%t)
// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name SelfImport -I %S/Inputs/SubmodulesSelfImport -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/SelfImport.symbols.json

// REQUIRES: objc_interop

// CHECK-DAG: "precise": "c:@F@parentFunc"
// CHECK-DAG: "precise": "c:@F@submoduleFunc"
