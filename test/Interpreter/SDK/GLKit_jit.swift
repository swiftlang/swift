// Test GLKit.swift in JIT mode.
// -module-name is necessary because of rdar://problem/14483032
// RUN: %target-jit-run -module-name=main %S/GLKit.swift | FileCheck %S/GLKit.swift

// REQUIRES: objc_interop
// REQUIRES: swift_interpreter
