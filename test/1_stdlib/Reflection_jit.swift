// Test Reflection.swift in JIT mode.
// RUN: %swift -interpret -sdk %sdk %S/Reflection.swift -- %S/Inputs/shuffle.jpg | FileCheck %S/Reflection.swift
// REQUIRES: sdk
// REQUIRES: swift_interpreter
