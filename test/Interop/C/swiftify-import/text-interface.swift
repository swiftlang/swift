// REQUIRES: swift_feature_StabilizedSafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -emit-module -emit-module-interface-path %t/test.swiftinterface -plugin-path %swift-plugin-dir -o %t/test.swiftmodule %t/test.swift -I %t -strict-memory-safety \
// RUN:   -verify -verify-additional-file %t%{fs-sep}test.h

// Verify that including the binary module works as expected
// RUN: %target-swift-frontend -swift-version 5 -emit-module -o %t/depender.swiftmodule %t/depender.swift -I %t -strict-memory-safety \
// RUN:   -verify -verify-additional-file %t%{fs-sep}test.h

// Make sure we trigger compilation from text interface
// RUN: rm %t/test.swiftmodule %t/depender.swiftmodule

// Comments are not emitted into the swiftinterface, so add some comments with the right path to depender.swift
// RUN: echo "// expected-textinterface-error@'%t%{fs-sep}test.swiftinterface':14{{cannot convert value of type 'UnsafeMutableBufferPointer<CInt>'}}" >> %t/depender.swift
// RUN: echo "// expected-textinterface-error@'%t%{fs-sep}test.swiftinterface':14{{missing argument for parameter #2 in call}}" >> %t/depender.swift

// RUN: echo "// expected-textinterface-error@'%t%{fs-sep}test.swiftinterface':20{{cannot convert value of type 'UnsafeMutablePointer<MutableSpan<CInt>>'}}" >> %t/depender.swift
// RUN: echo "// expected-textinterface-error@'%t%{fs-sep}test.swiftinterface':20{{missing argument for parameter #2 in call}}" >> %t/depender.swift

// Verify that absence of the macro plugin doesn't break compilation by itself - only if one of the inlinable functions calls the safe wrapper
// RUN: %target-swift-frontend -swift-version 5 -emit-module -o %t/depender.swiftmodule %t/depender.swift -I %t -strict-memory-safety \
// RUN:   -verify -verify-additional-file %t%{fs-sep}test.h -verify-additional-file %t%{fs-sep}test.swiftinterface -verify-additional-prefix textinterface- -suppress-notes


//--- test.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

// expected-textinterface-warning@+1{{could not load macro '_SwiftifyImport'; this may cause errors down the line}}
void bufferPointer(int *__counted_by(len), int len);
// expected-textinterface-warning@+1{{could not load macro '_SwiftifyImport'; this may cause errors down the line}}
void span(int *__counted_by(len) p __noescape, int len);

//--- test.swift
import Test

@inlinable
public func callBufferPointer(_ p: UnsafeMutablePointer<CInt>, _ len: CInt) {
  unsafe bufferPointer(p, len)
}

@inlinable
public func callBufferPointer(_ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe bufferPointer(p)
}

@inlinable
public func callSpan(_ p: UnsafeMutablePointer<CInt>, _ len: CInt) {
  unsafe span(p, len)
}

@inlinable
public func callSpan(_ p: inout MutableSpan<CInt>) {
  span(&p)
}

//--- depender.swift
// expected-textinterface-error@+1{{failed to build module 'test' for importation due to the errors above; the textual interface may be broken by project issues or a compiler bug}}
import test

func callCallBufferPointer(_ p: UnsafeMutablePointer<CInt>, len: CInt) {
  unsafe callBufferPointer(p, len)
}

func callCallBufferPointer(_ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe callBufferPointer(p)
}

func callCallSpan(_ p: UnsafeMutablePointer<CInt>, len: CInt) {
  unsafe callSpan(p, len)
}

func callCallSpan(_ p: inout MutableSpan<CInt>) {
  callSpan(&p)
}

//--- module.modulemap
module Test {
  header "test.h"
}

