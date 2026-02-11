// REQUIRES: swift_feature_StabilizedSafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs %t/succeed.swift
// RUN: not %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs %t/fail.swift -dump-source-file-imports 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs %t/fail.swift -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}B1.h -verify-ignore-macro-note

// Tests that we don't try to import modules that don't work well with Swift

// CHECK: imports for {{.*}}fail.swift:
// CHECK-NEXT: Swift
// CHECK-NEXT: _StringProcessing
// CHECK-NEXT: _SwiftConcurrencyShims
// CHECK-NEXT: _Concurrency
// CHECK-NEXT: B1
// CHECK-NEXT: A1

// CHECK-NEXT: imports for A1.foo:

// CHECK-NEXT: imports for @__swiftmacro{{.*}}foo{{.*}}_SwiftifyImport{{.*}}.swift:
// CHECK-NEXT: Swift
// CHECK-NEXT: B1
// CHECK-NEXT: _StringProcessing
// CHECK-NEXT: _SwiftConcurrencyShims
// CHECK-NEXT: _Concurrency

//--- Inputs/module.modulemap
module A1 {
  explicit module B1 {
    header "B1.h"
    explicit module C1 {
      header "C1.h"
      requires !swift
    }
  }
}

//--- Inputs/B1.h
#pragma once

#include "C1.h"
#define __sized_by(s) __attribute__((__sized_by__(s)))

// We can use bar without C1 causing errors
void bar(void * _Nonnull __sized_by(size), int size);
// foo causes an error when we try to refer to c1_t from the '!swift' module C1
c1_t foo(void * _Nonnull __sized_by(size), int size);
/*
expected-note@-2{{'foo' declared here}}
expected-expansion@-3:52{{
  expected-error@2:110{{cannot find type 'c1_t' in scope}}
}}
*/

//--- Inputs/C1.h
#pragma once

typedef int c1_t;

//--- fail.swift
import A1.B1

public func callUnsafe(_ p: UnsafeMutableRawPointer) {
  let _ = foo(p, 13)
}
public func callSafe(_ p: UnsafeMutableRawBufferPointer) {
  let _ = foo(p) // expected-error{{cannot convert value of type 'UnsafeMutableRawBufferPointer' to expected argument type 'UnsafeMutableRawPointer'}}
                 // expected-error@-1{{missing argument}}
}

//--- succeed.swift
import A1.B1

public func callUnsafe(_ p: UnsafeMutableRawPointer) {
  bar(p, 13)
}
public func callSafe(_ p: UnsafeMutableRawBufferPointer) {
  bar(p)
}
