// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -suppress-notes \
// RUN:   -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default \
// RUN:   %t%{fs-sep}test.swift
// RUN: %target-swift-frontend -typecheck -verify -suppress-notes \
// RUN:   -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default \
// RUN:   -strict-memory-safety -verify-additional-prefix strict- \
// RUN:   %t%{fs-sep}test.swift

// A C++ struct without a default constructor gets a synthesized init() that
// zero-fills its storage. That init() is unsafe, including when it satisfies a
// protocol's init() requirement.

//--- Inputs/module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- Inputs/test.h
struct NoDefaultCtor {
  explicit NoDefaultCtor(int x) : x(x) {}
  int x;
};

struct HasDefaultCtor {
  HasDefaultCtor() : x(1) {}
  int x;
};

//--- test.swift
import Test

protocol Zero { init() }

extension NoDefaultCtor: Zero {} // expected-strict-warning {{conformance of 'NoDefaultCtor' to protocol 'Zero' involves unsafe code}}
extension HasDefaultCtor: Zero {}

func direct() {
  _ = NoDefaultCtor() // expected-warning {{'init()' is deprecated}}
  // expected-strict-warning@-1 {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = HasDefaultCtor()
}
