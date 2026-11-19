// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: cp %t/Inputs/Original.h %t/Inputs/A.h
// RUN: cp %t/Inputs/Empty.h %t/Inputs/B.h
// RUN: %target-swift-frontend %t/Library.swift -I %t/Inputs -emit-module-path %t/Library.swiftmodule -cxx-interoperability-mode=default

// Move the specialization to another loaded Clang module.
// RUN: cp %t/Inputs/Empty.h %t/Inputs/A.h
// RUN: cp %t/Inputs/Original.h %t/Inputs/B.h
// RUN: not %target-swift-frontend -emit-sil %t/Library.swiftmodule -I %t/Inputs -diagnostic-style llvm -disable-workaround-broken-modules -cxx-interoperability-mode=default 2>&1 | %FileCheck %s --check-prefix=ERROR
// ERROR: error: reference to type 'Box.{{.*}}' broken by a context change; 'Box.{{.*}}' was expected to be in 'A', but now a candidate is found only in 'B'
// ERROR: note: could not deserialize extension

// RUN: %target-swift-frontend -emit-sil %t/Library.swiftmodule -I %t/Inputs -diagnostic-style llvm -cxx-interoperability-mode=default 2>&1 | %FileCheck %s --check-prefix=RECOVER
// RECOVER: warning: reference to type 'Box.{{.*}}' broken by a context change; 'Box.{{.*}}' was expected to be in 'A', but now a candidate is found only in 'B'
// RECOVER: note: attempting to recover from the previous modularization issue
// RECOVER: func makeBox() -> some Proto

// A different specialization of the same template must not satisfy the xref.
// RUN: cp %t/Inputs/OtherSpecialization.h %t/Inputs/B.h
// RUN: not %target-swift-frontend -emit-sil %t/Library.swiftmodule -I %t/Inputs -diagnostic-style llvm -cxx-interoperability-mode=default 2>&1 | %FileCheck %s --check-prefix=MISSING
// MISSING-NOT: warning:
// MISSING: error: reference to type 'Box.{{.*}}' broken by a context change; 'Box.{{.*}}' is not found, it was expected to be in 'A'
// MISSING: note: could not deserialize extension
// MISSING-NOT: attempting to recover

//--- Inputs/module.modulemap
module A {
  header "A.h"
  requires cplusplus
}
module B {
  header "B.h"
  requires cplusplus
}

//--- Inputs/Empty.h

//--- Inputs/Original.h
template <class T> struct Box { int marker = 0; };
using IntBox = Box<int>;

//--- Inputs/OtherSpecialization.h
template <class T> struct Box { int marker = 0; };
using DoubleBox = Box<double>;

//--- Library.swift
import A
import B

public protocol Proto {}
extension IntBox: Proto {}

public func makeBox() -> some Proto { IntBox() }
