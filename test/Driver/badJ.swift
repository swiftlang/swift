// RUN: not %swiftc_driver %S/../Inputs/empty.swift -jsnort 2>&1 |  %FileCheck %s
// CHECK-NOT: Stack dump
