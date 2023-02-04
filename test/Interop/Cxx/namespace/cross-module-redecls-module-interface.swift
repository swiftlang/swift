// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-ide-test -print-module -module-to-print=ModuleB -I %t/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck --check-prefix=CHECKB %s
// RUN: %target-swift-ide-test -print-module -module-to-print=ModuleA -I %t/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck --check-prefix=CHECKA %s

//--- Inputs/module.modulemap
module ModuleA {
    header "headerA.h"
    requires cplusplus
}

module ModuleB {
    header "headerB.h"
    requires cplusplus
}

//--- Inputs/headerA.h

namespace ns {
    struct A {
        int x;
    };
}

//--- Inputs/headerB.h

#include "headerA.h"

namespace ns {
    struct B {
        int y;
    };
}

// CHECKB:     enum ns {
// CHECKB-NEXT: struct B {
// CHECKB-NEXT:    init()
// CHECKB-NEXT:    init(y: Int32)
// CHECKB-NEXT:    var y: Int32
// CHECKB-NEXT:  }
// CHECKB-NEXT: }

// CHECKA:     enum ns {
// CHECKA-NEXT: struct A {
// CHECKA-NEXT:    init()
// CHECKA-NEXT:    init(x: Int32)
// CHECKA-NEXT:    var x: Int32
// CHECKA-NEXT:  }
// CHECKA-NEXT: }
