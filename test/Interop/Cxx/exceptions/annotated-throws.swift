// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/disabled.swift -I %t -cxx-interoperability-mode=default -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -emit-sil %t/use.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -strict-memory-safety -warnings-as-errors | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -emit-ir %t/use.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging | %FileCheck %s --check-prefix=IR

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- module.modulemap
module AnnotatedThrows {
  header "functions.h"
  requires cplusplus
}

//--- functions.h
#include <stdexcept>

#if defined(__swift__) && !defined(__swift_cxx_throws__)
#error "the Swift importer must advertise C++ exception bridging support"
#endif

#define SWIFT_THROWS __attribute__((swift_attr("import_throws")))

inline int checkedDivide(int numerator, int denominator) SWIFT_THROWS {
  if (!denominator)
    throw std::runtime_error("division by zero");
  return numerator / denominator;
}

inline void checkedVoid(bool fail) SWIFT_THROWS {
  if (fail)
    throw 42;
}

inline int unchanged(int value) { return value; }

namespace Numbers {
inline double checked(double value) SWIFT_THROWS {
  if (value < 0)
    throw std::runtime_error("negative floating point value");
  return value;
}
}
inline int checkedNoexcept(int value) noexcept SWIFT_THROWS { return value; }
inline int checkedUnnamed(int, int value) SWIFT_THROWS { return value; }
inline void failUnnamed(int, int) SWIFT_THROWS {
  throw std::runtime_error("unnamed parameters");
}
inline void violateNoexcept() noexcept SWIFT_THROWS {
  checkedVoid(true);
}

inline int cleanupCount = 0;
struct CountCleanup {
  ~CountCleanup() noexcept { ++cleanupCount; }
};
inline void throwWithCleanup() SWIFT_THROWS {
  CountCleanup cleanup;
  throw std::runtime_error("cleanup");
}
inline int getCleanupCount() noexcept { return cleanupCount; }
int checkedDefault(int value = 1) SWIFT_THROWS;

struct StaticFunctions {
  static int checked(int value) SWIFT_THROWS {
    if (value < 0)
      throw std::runtime_error("negative value");
    return value;
  }
};

struct Unsupported {
  int value;
  int instance() SWIFT_THROWS;
};
Unsupported aggregateResult() SWIFT_THROWS;
void aggregateParameter(Unsupported value) SWIFT_THROWS;
[[noreturn]] void neverReturns() SWIFT_THROWS;
enum class EnumResult { one = 1, two = 2 };
EnumResult enumResult() SWIFT_THROWS;

struct UnsupportedOperators {
  int operator*() const SWIFT_THROWS;
  UnsupportedOperators &operator++() SWIFT_THROWS;
  explicit operator bool() const SWIFT_THROWS;
  int operator[](int index) const SWIFT_THROWS;
};

//--- check.swift
import AnnotatedThrows

func check() throws {
  _ = checkedDivide(12, 3) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  checkedVoid(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = StaticFunctions.checked(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = unchanged(1)
  let _: (Double) throws -> Double = Numbers.checked
  _ = checkedNoexcept(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = checkedDefault() // expected-error {{'checkedDefault' is unavailable: SWIFT_THROWS on functions with default arguments is not yet supported}}
  let _: (CInt, CInt) throws -> CInt = checkedDivide
  let _: (Bool) throws -> Void = checkedVoid
  let _: (CInt) throws -> CInt = StaticFunctions.checked
  _ = aggregateResult() // expected-error {{'aggregateResult()' is unavailable: SWIFT_THROWS currently requires arithmetic or enum parameters and an arithmetic or void result}}
  aggregateParameter(Unsupported()) // expected-error {{'aggregateParameter' is unavailable: SWIFT_THROWS currently requires arithmetic or enum parameters and an arithmetic or void result}}
  _ = enumResult() // expected-error {{'enumResult()' is unavailable: SWIFT_THROWS currently requires arithmetic or enum parameters and an arithmetic or void result}}
  let operators = UnsupportedOperators()
  _ = operators.pointee // expected-error {{value of type 'UnsupportedOperators' has no member 'pointee'}}
  _ = operators.successor() // expected-error {{value of type 'UnsupportedOperators' has no member 'successor'}}
  _ = operators[1] // expected-error {{value of type 'UnsupportedOperators' has no subscripts}}
  var unsupported = Unsupported()
  _ = unsupported.instance() // expected-error {{'instance()' is unavailable: SWIFT_THROWS on instance methods is not yet supported}}
}

func checkNoReturn() {
  neverReturns() // expected-error {{'neverReturns()' is unavailable: SWIFT_THROWS is not supported on this kind of declaration}}
}

//--- disabled.swift
import AnnotatedThrows

func checkDisabled() {
  _ = checkedDivide(12, 3) // expected-error {{'checkedDivide' is unavailable: SWIFT_THROWS requires '-enable-experimental-feature CxxExceptionBridging'}}
  checkedVoid(false) // expected-error {{'checkedVoid' is unavailable: SWIFT_THROWS requires '-enable-experimental-feature CxxExceptionBridging'}}
}

//--- use.swift
import AnnotatedThrows

public func direct(_ numerator: CInt, _ denominator: CInt) throws -> CInt {
  return try checkedDivide(numerator, denominator)
}

public func captured() -> (CInt, CInt) throws -> CInt {
  return checkedDivide
}

public func voidResult(_ fail: Bool) throws {
  try checkedVoid(fail)
}

public func staticMethod(_ value: CInt) throws -> CInt {
  try StaticFunctions.checked(value)
}

// SIL: sil shared [transparent] {{.*}}checkedDivide{{.*}} : $@convention(thin) (Int32, Int32) -> (Int32, @error any Error)
// SIL: try_apply
// IR: define internal{{.*}} @{{.*}}__swift_cxx_exception_
// IR: invoke{{.*}} @_Z13checkedDivideii
// IR: catch ptr null
// IR: __swift_cxx_report_current_exception
