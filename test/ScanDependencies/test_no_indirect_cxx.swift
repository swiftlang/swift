// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/deps)
// RUN: split-file %s %t
// RUN: mv %t/Foo.swiftinterface %t/deps/

// RUN: %target-swift-frontend -scan-dependencies -o %t/deps.json %t/a.swift -I %t/deps -verify

//--- Foo.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Foo -cxx-interoperability-mode=default
public struct S1 {}

//--- a.swift
import Foo

// RUN: cat %t/deps.json | %FileCheck %s
// CHECK-NOT: "CxxShim"
