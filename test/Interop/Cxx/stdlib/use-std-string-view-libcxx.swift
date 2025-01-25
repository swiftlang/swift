// This test runs another test, use-std-string-view.swift, with libc++ explicitly specified as the C++ stdlib.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/use-std-string-view.swift -I %S/Inputs -o %t/exe -cxx-interoperability-mode=upcoming-swift -Xcc -stdlib=libc++
// RUN: %target-codesign %t/exe
// RUN: %target-run %t/exe

// REQUIRES: executable_test
// REQUIRES: OS=linux-gnu
// REQUIRES: system_wide_libcxx
