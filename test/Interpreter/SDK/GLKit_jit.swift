// Test GLKit.swift in JIT mode.
// RUN: %swift -interpret -sdk %sdk %S/GLKit.swift | FileCheck %S/GLKit.swift
// FIXME: broken on iOS
// REQUIRES: OS=macosx
// REQUIRES: sdk
// REQUIRES: swift_interpreter
