// Test protocol_lookup.swift in JIT mode.
// RUN: %target-jit-run %S/protocol_lookup.swift | %FileCheck %S/protocol_lookup.swift --check-prefix=CHECK --check-prefix=CHECK-%target-runtime
// REQUIRES: executable_test

// REQUIRES: swift_interpreter

