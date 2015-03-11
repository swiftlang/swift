// Test protocol_lookup.swift in JIT mode.
// RUN: %swift -interpret %S/protocol_lookup.swift | FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

// REQUIRES: swift_interpreter

