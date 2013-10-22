// REQUIRES: asserts

// RUN: %swift -emit-llvm %S/Inputs/single_int.swift -o /dev/null -print-stats 2>&1 | FileCheck %s -check-prefix=STATS
// STATS: Statistics Collected
// Only print statistics once
// STATS-NOT: Statistics Collected
