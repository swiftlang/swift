// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: rm -rf %t && mkdir %t
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | FileCheck %s

1
// CHECK: {{\[}}[[@LINE-1]]:1-[[@LINE-1]]:2] $builtin_log[='1']
