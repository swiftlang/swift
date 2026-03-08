// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend -typecheck -import-bridging-header %t/test.h -verify -verify-ignore-unrelated %t/test.swift 2> %t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %diff %t/test.swift %t/test.swift.expected
// RUN: %target-swift-frontend -typecheck -import-bridging-header %t/test.h -verify -verify-ignore-unrelated %t/test.swift
// RUN: not %target-swift-frontend -typecheck -import-bridging-header %t/test.h -verify %t/test.swift

//--- test.h
static inline int bar() {
  return 0/0;
}

//--- test.swift
func foo() {
  a = 2
  let b = bar()
}

//--- test.swift.expected
func foo() {
  // expected-error@+1{{cannot find 'a' in scope}}
  a = 2
  let b = bar()
}

