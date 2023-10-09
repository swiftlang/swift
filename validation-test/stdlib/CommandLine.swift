// RUN: %empty-directory(%t)
//
//
// RUN: %target-build-swift %S/Inputs/CommandLineStressTest/CommandLineStressTest.swift -parse-as-library -whole-module-optimization -module-name CommandLineStressTestSwift -emit-object -o %t/CommandLineStressTestSwift.o
// RUN: %clang -arch %target-cpu -c -o %t/CommandLineStressTest.o -x c %S/Inputs/CommandLineStressTest/CommandLineStressTest.c
// RUN: %target-build-swift %t/CommandLineStressTest.o %t/CommandLineStressTestSwift.o -o %t/CommandLineStressTest
// RUN: %target-codesign %t/CommandLineStressTest
// RUN: %target-run %t/CommandLineStressTest foo bar baz qux quux corge grault garply waldo fred plugh xyzzy and thud
// REQUIRES: executable_test
// REQUIRES: stress_test
// UNSUPPORTED: threading_none

// REQUIRES: rdar70423908

// This file is an empty stub to call into the command line stress test which
// houses `main`.
