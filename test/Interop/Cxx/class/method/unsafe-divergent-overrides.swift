// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -suppress-notes \
// RUN:   -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default

// Regression test: when a virtual method overrides multiple base methods whose
// imported Swift names diverge, every cloned base member in the derived class
// must be marked unavailable, including ones whose Swift name was rewritten by
// the `__<base>Unsafe` rename applied to unsafe projection methods.

//--- Inputs/module.modulemap
module UnsafeDivergentOverrides {
  header "unsafe-divergent-overrides.h"
  requires cplusplus
}

//--- Inputs/unsafe-divergent-overrides.h
#ifndef TEST_INTEROP_CXX_CLASS_METHOD_UNSAFE_DIVERGENT_OVERRIDES_H
#define TEST_INTEROP_CXX_CLASS_METHOD_UNSAFE_DIVERGENT_OVERRIDES_H

struct UnsafeReturn {
  void *ptr;
};

struct SelfContainedBase1 {
  void *ptr;
  SelfContainedBase1(const SelfContainedBase1 &);
  virtual ~SelfContainedBase1();

  virtual UnsafeReturn view() const;
};

struct SelfContainedBase2 {
  void *ptr;
  SelfContainedBase2(const SelfContainedBase2 &);
  virtual ~SelfContainedBase2();

  virtual UnsafeReturn view() const __attribute__((swift_name("getView()")));
};

struct DerivedDivergent : SelfContainedBase1, SelfContainedBase2 {
  UnsafeReturn view() const override;
};

#endif // TEST_INTEROP_CXX_CLASS_METHOD_UNSAFE_DIVERGENT_OVERRIDES_H

//--- test.swift

// Both cloned base members must be marked unavailable on the derived class,
// including the one renamed to `__viewUnsafe` by the unsafe-projection rename.

import UnsafeDivergentOverrides

func use(_ d: DerivedDivergent) {
  let _ = d.getView()  // expected-error {{'getView()' is unavailable: overrides multiple C++ methods with different Swift names}}
  let _ = d.__viewUnsafe()  // expected-error {{'__viewUnsafe()' is unavailable: overrides multiple C++ methods with different Swift names}}
}
