// A basic test for debug info.
// UNSUPPORTED: OS=windows-msvc
// --------------------------------------------------------------------
// Verify that we don't emit any debug info by default. This test has a
// specific Windows implementation in debug_basic_windows.swift.
// RUN: %target-swift-frontend %s -emit-ir -o - \
// RUN:   | %FileCheck %s --check-prefix NDEBUG
// NDEBUG: source_filename
// NDEBUG-NOT: !dbg
// NDEBUG-NOT: DICompileUnit
// --------------------------------------------------------------------
// Verify that we don't emit any debug info with -gnone.
// RUN: %target-swift-frontend %s -emit-ir -gnone -o - \
// RUN:   | %FileCheck %s --check-prefix NDEBUG
public func foo() -> Int {
  let bar = 1
  return bar
}