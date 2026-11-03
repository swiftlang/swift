// RUN: %empty-directory(%t)
// RUN: split-file %S/shared-stdlib-header.swift %t

// The standalone program uses the header built with the runtime and has no
// Swift source or generated user-module header.
// RUN: %target-interop-build-clangxx -std=c++17 -fno-exceptions -c %t/standalone.cpp -o %t/standalone.o
// RUN: %target-build-swift %t/standalone.o -o %t/standalone -lswiftCore %target-cxx-lib
// RUN: %target-codesign %t/standalone
// RUN: %target-run %t/standalone

// RUN: %target-swift-frontend %t/Record.swift -module-name Record -parse-as-library -emit-object -o %t/Record.o -cxx-interoperability-mode=default -emit-clang-header-path %t/Record.h
// RUN: %target-swift-frontend %t/Text.swift -module-name Text -parse-as-library -emit-object -o %t/Text.o -cxx-interoperability-mode=default -emit-clang-header-path %t/Text.h

// RUN: %target-interop-build-clangxx -std=c++17 -fno-exceptions -c %t/mixed.cpp -I %t -DSWIFT_HEADER_FIRST -o %t/mixed-first.o
// RUN: %target-build-swift %t/mixed-first.o %t/Record.o %t/Text.o -o %t/mixed-first %target-cxx-lib
// RUN: %target-codesign %t/mixed-first
// RUN: %target-run %t/mixed-first

// RUN: %target-interop-build-clangxx -std=c++20 -O -c %t/mixed.cpp -I %t -o %t/mixed-last.o
// RUN: %target-build-swift %t/mixed-last.o %t/Record.o %t/Text.o -o %t/mixed-last %target-cxx-lib
// RUN: %target-codesign %t/mixed-last
// RUN: %target-run %t/mixed-last

// REQUIRES: executable_test
