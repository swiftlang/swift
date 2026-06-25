// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t %t/test.swift \
// RUN:   -verify -verify-ignore-unrelated -Rmacro-expansions 2> %t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t %t/test.swift \
// RUN:   -verify -verify-ignore-unrelated -Rmacro-expansions
// RUN: %diff %t/test.swift %t/test.swift.expected

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
//--- test.swift.expected
import Unrelated

func bar(_ s: Span<CInt>) {
  // expected-error@+1{{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'UnsafeBufferPointer<Int32>'}}
  foo(s)
}
