// RUN: rm -rf %t
// RUN: mkdir -p %t
//
//
// RUN: %target-build-swift %S/Inputs/CommandLineStressTest/CommandLineStressTest.swift -parse-as-library -force-single-frontend-invocation -module-name CommandLineStressTestSwift -emit-object -o %t/CommandLineStressTestSwift.o
// RUN: %clang -arch %target-cpu -c -o %t/CommandLineStressTest.o -x c %S/Inputs/CommandLineStressTest/CommandLineStressTest.c
// RUN: %target-build-swift %t/CommandLineStressTest.o %t/CommandLineStressTestSwift.o -o %t/CommandLineStressTest
// RUN: %target-run %t/CommandLineStressTest foo bar baz qux quux corge grault garply waldo fred plugh xyzzy and thud
// REQUIRES: executable_test
// UNSUPPORTED: nonatomic_rc

// This file is an empty stub to call into the command line stress test which
// houses `main`.
