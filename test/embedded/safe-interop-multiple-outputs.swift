// Regression test: compiler crashes in IRGenerator::getGenModule
// when calling a safe wrapper from an Embedded module compiled
// with multiple source files and multiple threads, without WMO.
// https://github.com/swiftlang/swift/issues/88864

// REQUIRES: swift_feature_Embedded

// XFAIL: *

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -c -plugin-path %swift-plugin-dir -I %t \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -parse-as-library -num-threads 2 \
// RUN:   %t/A.swift %t/B.swift \
// RUN:   -o %t/A.swift.o -o %t/B.swift.o

//--- test.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

void foo(const int *__counted_by(len) p __noescape, int len);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- A.swift
import Test

public func bar(_ s: Span<Int32>) {
    foo(s)
}

//--- B.swift
public struct Baz {}
