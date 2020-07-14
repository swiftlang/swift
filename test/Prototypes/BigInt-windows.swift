// This test is intentionally specialized on windows because `not --crash`
// behaves differently there: the exit code from the crashing frontend
// invocations is mapped to `-21` there, which lit's `not` along does not remap
// correctly.
// See rdar://problem/65251059
// REQUIRES: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: not --crash %target-build-swift -swift-version 4 -o %t/a.out %S/BigInt.swift
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: CPU=x86_64
// XFAIL: asserts
