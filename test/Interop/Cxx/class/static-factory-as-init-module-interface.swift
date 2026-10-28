// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -print-module -module-to-print=StaticFactoryAsInit -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default > %t/interface.txt

// RUN: %FileCheck --check-prefix=FROM-INT %s < %t/interface.txt
// RUN: %FileCheck --check-prefix=DEFAULTED-TEMPLATE %s < %t/interface.txt
// RUN: %FileCheck --check-prefix=OVERLOAD %s < %t/interface.txt
// RUN: %FileCheck --check-prefix=OVERLOAD-EXTRA %s < %t/interface.txt
// RUN: %FileCheck --check-prefix=REAL-CTOR %s < %t/interface.txt
// RUN: %FileCheck --check-prefix=FROM-FACTORY %s < %t/interface.txt

// Each spelling gets its own FileCheck prefix so that the assertions do not
// depend on the order in which members are printed.

// FROM-INT-COUNT-1: init(fromInt v: CInt)
// FROM-INT-NOT:     init(fromInt v: CInt)

// DEFAULTED-TEMPLATE-COUNT-1: init(fromDefaultedTemplate v: CInt)
// DEFAULTED-TEMPLATE-NOT:     init(fromDefaultedTemplate v: CInt)

// OVERLOAD-COUNT-1: init(overload v: CInt)
// OVERLOAD-NOT:     init(overload v: CInt)

// OVERLOAD-EXTRA-COUNT-1: init(overload v: CInt, extra w: CInt)
// OVERLOAD-EXTRA-NOT:     init(overload v: CInt, extra w: CInt)

// REAL-CTOR-COUNT-1: init(_ v: CInt)
// REAL-CTOR-NOT:     init(_ v: CInt)

// FROM-FACTORY-COUNT-1: init(fromFactory v: CInt)
// FROM-FACTORY-NOT:     init(fromFactory v: CInt)
