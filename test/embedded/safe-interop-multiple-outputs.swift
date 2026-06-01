// Regression test: this used to trigger a compiler crash in IRGenerator::getGenModule
// since the SourceFile for the _SwiftifyImport macro expansion had no corresponding
// IRGenModule. This would only trigger with multiple outputs and multiple threads
// since there's only a single IRGenModule to return otherwise.

// REQUIRES: swift_feature_Embedded

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
