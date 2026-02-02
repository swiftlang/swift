// REQUIRES: OS=windows-msvc

// Test that a struct named _GUID is found and usable in C++ when it is
// defined, not just forward declared, within the same Clang module where
// _GUID is specially-handled as a predefined (forward) declaration on
// Windows.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -c -primary-file %t/Test.swift -cxx-interoperability-mode=default -I %t -Xcc -fmodule-map-file=%t/module.modulemap -Xcc -I -Xcc %t -module-name Test -o %t/Test.swift.o -module-cache-path %t/module-cache

//--- Test.swift
import TestMod

extension TestMod._GUID {
  public func m() -> Int { 42 }
}

//--- test.h
#ifndef TEST_H
#define TEST_H

typedef struct _GUID {
  int Data;
} GUID;

#endif // TEST_H

//--- module.modulemap
module TestMod {
  header "test.h"
}
