// RUN: %empty-directory(%t)
// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name FactoryInit -F %S/Inputs/FactoryInit -output-dir %t -pretty-print -v

// REQUIRES: objc_interop

// RUN: %FileCheck %s --input-file %t/FactoryInit.symbols.json

// CHECK-NOT: not inherited
