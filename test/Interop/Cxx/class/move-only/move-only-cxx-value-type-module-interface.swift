// RUN: %target-swift-ide-test -print-module -module-to-print=MoveOnlyCxxValueType -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -source-filename=x | %FileCheck %s --check-prefix=CHECK-NCG

// CHECK-NCG: func getNonCopyablePtr() -> UnsafeMutablePointer<NonCopyable>
// CHECK-NCG: func getNonCopyableDerivedPtr() -> UnsafeMutablePointer<NonCopyableDerived>
