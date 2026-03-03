// RUN: %empty-directory(%t)

// RUN: split-file %s %t

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name UmbrellaSubmodules -I %t/UmbrellaSubmodules -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/UmbrellaSubmodules.symbols.json

// REQUIRES: objc_interop

//--- UmbrellaSubmodules/module.modulemap
module UmbrellaSubmodules {
    umbrella header "UmbrellaSubmodules.h"

    export *
    module * {
        export *
    }
}

//--- UmbrellaSubmodules/UmbrellaSubmodules.h
#include "HeaderOne.h"
#include "HeaderTwo.h"
// CHECK-DAG: "precise": "c:UmbrellaSubmodules.h@umbrellaVar"
static int umbrellaVar = 0;

//--- UmbrellaSubmodules/HeaderOne.h
// CHECK-DAG: "precise": "c:HeaderOne.h@varOne"
static int varOne = 1;

//--- UmbrellaSubmodules/HeaderTwo.h
// CHECK-DAG: "precise": "c:HeaderTwo.h@varTwo"
static int varTwo = 2;
