// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging -verify -verify-ignore-unrelated
// RUN: %target-build-swift %t/main.swift -I %t -o %t/test -cxx-interoperability-mode=default -Xfrontend -cxx-exception-mode=strict -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test
// RUN: %target-build-swift %t/main.swift -I %t -o %t/test-opt -O -cxx-interoperability-mode=default -Xfrontend -cxx-exception-mode=strict -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- module.modulemap
module StrictMembers {
  header "members.h"
  requires cplusplus
}

//--- members.h
#include <stdexcept>
inline int checkedMemberValue(int value) {
  if (value < 0)
    throw std::runtime_error("negative member value");
  return value;
}
struct Value {
  int value;
  Value(int value) : value(checkedMemberValue(value)) {}
  int read() const { return checkedMemberValue(value); }
  void update(int next) { value = checkedMemberValue(next); }
  int consume() && { return checkedMemberValue(value); }
  int safeRead() const noexcept { return value; }
};
struct Derived : Value { using Value::Value; };
inline int throwingDefault() { throw std::runtime_error("default member"); }
struct Defaulted {
  int value = throwingDefault();
  Defaulted() = default;
};
struct ImplicitDefaulted {
  int value = throwingDefault();
};
struct NoThrowDefault { int value = 17; };
struct DefaultArgument {
  int value;
  DefaultArgument(int value = throwingDefault()) noexcept : value(value) {}
};
struct __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:immortal")))
    __attribute__((swift_attr("release:immortal"))) NoThrowReference {
  explicit NoThrowReference(int) noexcept {}
};

//--- check.swift
import StrictMembers
func check(_ value: inout Value) throws {
  _ = Value(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  _ = value.read() // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  value.update(2) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  _ = value.safeRead()
  _ = NoThrowDefault()
  _ = ImplicitDefaulted() // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'}} expected-note {{did you mean to handle error as optional value}} expected-note {{did you mean to disable error propagation}}
  _ = DefaultArgument() // expected-error {{missing argument for parameter #1 in call}}
}

@available(macOS 13.3, *)
func checkReference() {
  // The synthesized factory may allocate even when its constructor is noexcept.
  _ = NoThrowReference(1) // expected-error {{'init(_:)' is unavailable: C++ exception bridging is not supported on this kind of declaration}}
}

//--- main.swift
import Cxx
import StrictMembers

var value = try Value(42)
let read = try value.read()
precondition(read == 42)
try value.update(7)
precondition(value.safeRead() == 7)
let capturedRead = value.read
let captured = try capturedRead()
precondition(captured == 7)
let consumed = try value.consume()
precondition(consumed == 7)
let inheritedConstructor: (CInt) throws -> Derived = Derived.init
let derived = try inheritedConstructor(19)
let inheritedRead = try derived.read()
precondition(inheritedRead == 19)
precondition(NoThrowDefault().value == 17)
precondition(DefaultArgument(23).value == 23)

do {
  _ = try Value(-1)
  fatalError("expected constructor failure")
} catch let error as CxxException {
  precondition(error.message == "negative member value")
}
do {
  try value.update(-1)
  fatalError("expected method failure")
} catch let error as CxxException {
  precondition(error.message == "negative member value")
  precondition(value.safeRead() == 7)
}
do {
  _ = try Defaulted()
  fatalError("expected default member initializer failure")
} catch let error as CxxException {
  precondition(error.message == "default member")
}
do {
  _ = try ImplicitDefaulted()
  fatalError("expected implicit default constructor failure")
} catch let error as CxxException {
  precondition(error.message == "default member")
}
