// RUN: %empty-directory(%t)

// RUN: split-file %s %t

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name PartialSubmoduleExport -I %t/PartialSubmoduleExport -output-dir %t -pretty-print -v

// RUN: %FileCheck %s --input-file %t/PartialSubmoduleExport.symbols.json
// check the missing symbols separately to account for arbitrary ordering
// RUN: %FileCheck %s --input-file %t/PartialSubmoduleExport.symbols.json --check-prefix MISSING

// REQUIRES: objc_interop

// The PartialSubmoduleExport module below is structured like this:

// PartialSubmoduleExport
// - GroupA
//   - GroupAOne
//   - GroupATwo
// - GroupB
//   - GroupBOne
//   - GroupBTwo

// The module map then exports `GroupA.*` and `GroupB.GroupBOne` explicitly.
// This test ensures that the expected symbols are in the module map,
// and that the deliberately excluded `groupBTwo` symbol is left out.

//--- PartialSubmoduleExport/module.modulemap
module PartialSubmoduleExport {
    header "PartialSubmoduleExport.h"

    explicit module GroupA {
        umbrella header "GroupA/GroupA.h"

        module * { export * }
    }

    explicit module GroupB {
        umbrella header "GroupB/GroupB.h"

        module * { export * }
    }

    export GroupA.*
    export GroupB.GroupBOne
}

//--- PartialSubmoduleExport/PartialSubmoduleExport.h
#include "GroupA/GroupA.h"
#include "GroupB/GroupB.h"

// CHECK-DAG: "precise": "c:PartialSubmoduleExport.h@umbrellaVar"
static int umbrellaVar = 0;

//--- PartialSubmoduleExport/GroupA/GroupA.h
#include "GroupAOne.h"
#include "GroupATwo.h"

// CHECK-DAG: "precise": "c:GroupA.h@groupAVar"
static int groupAVar = 0;

//--- PartialSubmoduleExport/GroupA/GroupAOne.h
// CHECK-DAG: "precise": "c:GroupAOne.h@groupAOne"
static int groupAOne = 1;

//--- PartialSubmoduleExport/GroupA/GroupATwo.h
// CHECK-DAG: "precise": "c:GroupATwo.h@groupATwo"
static int groupATwo = 2;

//--- PartialSubmoduleExport/GroupB/GroupB.h
#include "GroupBOne.h"
#include "GroupBTwo.h"

// Because GroupB was not exported by itself, this symbol should be missing
// MISSING-NOT: "precise": "c:GroupB.h@groupBVar"
static int groupBVar = 0;

//--- PartialSubmoduleExport/GroupB/GroupBOne.h
// CHECK-DAG: "precise": "c:GroupBOne.h@groupBOne"
static int groupBOne = 1;

//--- PartialSubmoduleExport/GroupB/GroupBTwo.h
// Because GroupBTwo is not exported in the top-level module map,
// this shouldn't be in the symbol graph
// MISSING-NOT: "precise": "c:GroupBTwo.h@groupBTwo"
static int groupBTwo = 2;
