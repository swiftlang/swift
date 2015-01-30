// Test Reflection.swift in JIT mode.
// RUN: %swift -interpret -sdk %sdk %S/Reflection.swift -- %S/Inputs/shuffle.jpg | FileCheck %S/Reflection.swift

// XFAIL: linux
// REQUIRES: swift_interpreter
