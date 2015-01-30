// Test GLKit.swift in JIT mode.
// -module-name is necessary because of rdar://problem/14483032
// RUN: %swift -interpret -module-name=main -sdk %sdk %S/GLKit.swift | FileCheck %S/GLKit.swift

// REQUIRES: objc_interop
// REQUIRES: swift_interpreter
