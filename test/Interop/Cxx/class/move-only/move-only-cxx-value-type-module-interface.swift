// RUN: %target-swift-ide-test -print-module -module-to-print=MoveOnlyCxxValueType -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -source-filename=x | %FileCheck %s

// CHECK: func getNonCopyablePtr() -> UnsafeMutablePointer<NonCopyable>
// CHECK: func getNonCopyableDerivedPtr() -> UnsafeMutablePointer<NonCopyableDerived>
