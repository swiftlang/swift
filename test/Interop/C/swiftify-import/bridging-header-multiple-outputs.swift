// Regression test: this used to trigger a compiler crash in IRGenerator::getGenModule
// since the SourceFile for the _SwiftifyImport macro expansion had no corresponding
// IRGenModule. This would only trigger with multiple outputs and multiple threads
// since there's only a single IRGenModule to return otherwise.
// This is the bridging header, non-embedded, counterpart of
// test/embedded/safe-interop-multiple-outputs.swift, which covers the same crash
// when the annotated function is imported as a clang module instead.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -c -plugin-path %swift-plugin-dir \
// RUN:   -import-bridging-header %t/bridging.h \
// RUN:   -parse-as-library -num-threads 2 \
// RUN:   %t/A.swift %t/B.swift \
// RUN:   -o %t/A.swift.o -o %t/B.swift.o

//--- bridging.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))

void foo(const int *__counted_by(len) p, int len);

//--- A.swift
public func bar(_ s: UnsafeBufferPointer<CInt>) {
    foo(s)
}

//--- B.swift
public struct Baz {}
