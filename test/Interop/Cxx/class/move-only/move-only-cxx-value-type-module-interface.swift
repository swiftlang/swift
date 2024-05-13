// RUN: %target-swift-ide-test -print-module -module-to-print=MoveOnlyCxxValueType -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -source-filename=x | %FileCheck %s --check-prefix=CHECK

// CHECK: func getNonCopyablePtr() -> OpaquePointer
// CHECK: func getNonCopyableDerivedPtr() -> OpaquePointer

// FIXME: would prefer to have this (rdar://128013193)
// func getNonCopyablePtr() -> UnsafeMutablePointer<NonCopyable>
// func getNonCopyableDerivedPtr() -> UnsafeMutablePointer<NonCopyableDerived>
