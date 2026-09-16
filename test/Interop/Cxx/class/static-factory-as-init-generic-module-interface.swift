// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -print-module -module-to-print=StaticFactoryAsInitGeneric -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default > %t/interface.txt

// RUN: %FileCheck --check-prefix=ONE-PARAM %s < %t/interface.txt
// RUN: %FileCheck --check-prefix=TWO-PARAMS %s < %t/interface.txt
// RUN: %FileCheck --check-prefix=MIXED %s < %t/interface.txt

// Each spelling gets its own FileCheck prefix so the assertions do not depend on
// member print order.

// ONE-PARAM-COUNT-1: init<T>(fromGeneric v: T)
// ONE-PARAM-NOT:     init<T>(fromGeneric v: T)

// TWO-PARAMS-COUNT-1: init<T, U>(_ v: T, other w: U)
// TWO-PARAMS-NOT:     init<T, U>(_ v: T, other w: U)

// MIXED-COUNT-1: init<T>(generic v: T, concrete w: CInt)
// MIXED-NOT:     init<T>(generic v: T, concrete w: CInt)
