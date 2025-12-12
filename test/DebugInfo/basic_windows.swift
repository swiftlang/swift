// A basic test for debug info.
// REQUIRES: OS=windows-msvc
// --------------------------------------------------------------------
// Verify that, by default, we exactly emit 1 DICompileUnit entry without dwoId.
// On Windows, we always emit minimal DebugInfo to match MSVC's behavior:
// See https://github.com/llvm/llvm-project/pull/142970 for more details.
// RUN: %target-swift-frontend %s -emit-ir -o - \
// RUN:   | %FileCheck %s --check-prefix NDEBUG
// NDEBUG: source_filename
// NDEBUG: !DICompileUnit(
// NDEBUG-NOT: dwoId:
// NDEBUG-SAME: )
// NDEBUG-NOT: !DICompileUnit(
public func foo() -> Int {
  let bar = 1
  return bar
}
