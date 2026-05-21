// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t %t/test.swift \
// RUN:   -verify -verify-ignore-unrelated -Rmacro-expansions 2> %t/output.txt
// RUN: not %update-verify-tests < %t/output.txt 2>&1 | %FileCheck %s

// CHECK: Error in update-verify-tests while parsing tool output: unhandled note found
// CHECK-SAME: in expansion from here

//--- module.modulemap
module Unrelated {
  header "unrelated.h"
  export *
}

//--- unrelated.h
#define __counted_by(x) __attribute__((__counted_by__(x)))

void foo(const int * __counted_by(len) p, int len);

//--- test.swift
import Unrelated

func bar(_ s: Span<CInt>) {
  foo(s)
}
