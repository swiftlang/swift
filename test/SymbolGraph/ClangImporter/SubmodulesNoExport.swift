// RUN: %empty-directory(%t)

// RUN: split-file %s %t

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name SubmodulesNoExport -I %t/SubmodulesNoExport -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/SubmodulesNoExport.symbols.json

// REQUIRES: objc_interop

// Make sure that a Clang module that declares submodules but doesn't have an export declaration
// still contains all the submodules' symbols.

//--- SubmodulesNoExport/module.modulemap
module SubmodulesNoExport {
    header "SubmodulesNoExport.h"

    module HeaderOne {
        header "HeaderOne.h"
        export *
    }
    module HeaderTwo {
        header "HeaderTwo.h"
        export *
    }
}

//--- SubmodulesNoExport/SubmodulesNoExport.h
#include "HeaderOne.h"
#include "HeaderTwo.h"
// CHECK-DAG: "precise": "c:SubmodulesNoExport.h@umbrellaVar"
static int umbrellaVar = 0;

//--- SubmodulesNoExport/HeaderOne.h
// CHECK-DAG: "precise": "c:HeaderOne.h@varOne"
static int varOne = 1;

//--- SubmodulesNoExport/HeaderTwo.h
// CHECK-DAG: "precise": "c:HeaderTwo.h@varTwo"
static int varTwo = 2;

