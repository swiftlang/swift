// RUN: %empty-directory(%t)

// RUN: split-file %s %t

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name UmbrellaNoExport -I %t/UmbrellaNoExport -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/UmbrellaNoExport.symbols.json

// REQUIRES: objc_interop

//--- UmbrellaNoExport/module.modulemap
module UmbrellaNoExport {
    umbrella header "UmbrellaNoExport.h"

    module * {
        export *
    }
}

//--- UmbrellaNoExport/UmbrellaNoExport.h
#include "HeaderOne.h"
#include "HeaderTwo.h"
// CHECK-DAG: "precise": "c:UmbrellaNoExport.h@umbrellaVar"
static int umbrellaVar = 0;

//--- UmbrellaNoExport/HeaderOne.h
// CHECK-DAG: "precise": "c:HeaderOne.h@varOne"
static int varOne = 1;

//--- UmbrellaNoExport/HeaderTwo.h
// CHECK-DAG: "precise": "c:HeaderTwo.h@varTwo"
static int varTwo = 2;

