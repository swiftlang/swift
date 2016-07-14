// RUN: rm -rf %t
// RUN: mkdir -p %t
//
//
// RUN: %target-build-swift %S/Inputs/ProcessStressTest/ProcessStressTest.swift -parse-as-library -force-single-frontend-invocation -module-name ProcessStressTestSwift -emit-object -o %t/ProcessStressTestSwift.o
// RUN: %clang -arch %target-cpu -c -o %t/ProcessStressTest.o -x c %S/Inputs/ProcessStressTest/ProcessStressTest.c
// RUN: %target-build-swift %t/ProcessStressTest.o %t/ProcessStressTestSwift.o -o %t/ProcessStressTest
// RUN: %target-run %t/ProcessStressTest foo bar baz qux quux corge grault garply waldo fred plugh xyzzy and thud
// REQUIRES: executable_test

// This file is an empty stub to call into the process stress test which
// houses `main`.
