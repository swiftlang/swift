// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %t/Inputs %t/test.swift -cxx-interoperability-mode=default -diagnostic-style llvm

// A C++ API annotated with swift_attr("unsafe(always)") must have its uses
// acknowledged with 'unsafe' even without -strict-memory-safety.

//--- Inputs/module.modulemap
module AlwaysUnsafeCxx {
    header "always-unsafe.h"
    requires cplusplus
}

//--- Inputs/always-unsafe.h
struct Widget {
  int value;

  // The lifetime requirements of this API cannot be expressed in Swift, so
  // every use has to be audited by hand.
  __attribute__((swift_attr("unsafe(always)")))
  const int *dangerousPointer() const { return &value; }

  __attribute__((swift_attr("unsafe")))
  int trickyValue() const { return value; }
};

// A whole record can be always-unsafe too.
struct __attribute__((swift_attr("unsafe(always)"))) DangerousWidget {
  int value;
};

// Both spellings on one declaration: the stronger one wins, in either order.
struct BothSpellings {
  __attribute__((swift_attr("unsafe(always)")))
  __attribute__((swift_attr("unsafe")))
  int alwaysFirst() const { return 1; }

  __attribute__((swift_attr("unsafe")))
  __attribute__((swift_attr("unsafe(always)")))
  int alwaysSecond() const { return 2; }
};

//--- test.swift
import AlwaysUnsafeCxx

func test(w: Widget) {
  _ = w.dangerousPointer()
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2{{reference to unsafe instance method 'dangerousPointer()'}}

  _ = unsafe w.dangerousPointer()

  // A merely unsafe C++ API needs no acknowledgement here.
  _ = w.trickyValue()
}

func testRecord(d: DangerousWidget) {
  _ = d
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2{{reference to parameter 'd' involves unsafe type 'DangerousWidget'}}

  _ = unsafe d
}

func testBothSpellings(b: BothSpellings) {
  _ = b.alwaysFirst()
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2{{reference to unsafe instance method 'alwaysFirst()'}}

  _ = b.alwaysSecond()
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2{{reference to unsafe instance method 'alwaysSecond()'}}

  _ = unsafe b.alwaysFirst()
  _ = unsafe b.alwaysSecond()
}
