// Test Reflection.swift in JIT mode.
// RUN: %target-jit-run -parse-stdlib %S/Reflection.swift -- %S/Inputs/shuffle.jpg | %FileCheck %S/Reflection.swift

// REQUIRES: swift_interpreter

// Only run this test when we build host tools (e.g. bin/swift-frontend), avoid running it for standalone stdlib builds.
// Standalone stdlib builds use downloadable toolchains from swift.org, which are codesigned in a way that doesn't let
// the interpreter process (swift-frontend) dynamically load locally-built modules (libswiftCore).
// REQUIRES: swift_tools_extra
