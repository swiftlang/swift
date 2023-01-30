// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-ide-test -print-module -module-to-print=CxxModule -I %t/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-experimental-feature ImportSymbolicCXXDecls | %FileCheck %s

// REQUIRES: asserts

//--- Inputs/module.modulemap
module CxxModule {
    header "headerA.h"
    header "headerB.h"
    requires cplusplus
}

//--- Inputs/headerA.h

namespace ns {
    int freeFunction(int x, int y);
}

//--- Inputs/headerB.h

#include "headerA.h"

namespace ns {
    struct B {
        int y;
    };

    template<class T>
    struct TemplateRecord {
        void methodFunc(int x);

        struct InnerRecord {
            int innerMethod(int y);
        };

        template<class T2>
        struct InnerTemplate {
            void innerTemplateMethod();
        };

        InnerTemplate<int> returnsTemplateMethod();
    };
}

using MyType = ns::TemplateRecord<int>;

// CHECK:     enum ns {
// CHECK-NEXT: struct B {
// CHECK-NEXT:    init()
// CHECK-NEXT:    init(y: Int32)
// CHECK-NEXT:    var y: Int32
// CHECK-NEXT:  }
// CHECK-NEXT:  struct TemplateRecord {
// CHECK-NEXT:    mutating func methodFunc()
// CHECK-NEXT:    struct InnerRecord {
// CHECK-NEXT:      mutating func innerMethod()
// CHECK-NEXT:    }
// CHECK-NEXT:    struct InnerTemplate {
// CHECK-NEXT:      mutating func innerTemplateMethod()
// CHECK-NEXT:    }
// CHECK-NEXT:    mutating func returnsTemplateMethod()
// CHECK-NEXT:  }
// CHECK-NEXT:  static func freeFunction(_ x: Int32, _ y: Int32) -> Int32
// CHECK-NEXT: }
// CHECK-NEXT: typealias MyType = ns.TemplateRecord
