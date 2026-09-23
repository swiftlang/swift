// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -cxx-interop-getters-setters-as-properties -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -emit-sil %t/use.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -strict-memory-safety -warnings-as-errors | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -emit-ir %t/use.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging | %FileCheck %s --check-prefix=IR

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- module.modulemap
module AnnotatedThrowingMethods {
  header "methods.h"
  requires cplusplus
}

//--- methods.h
#include <stdexcept>
#define SWIFT_THROWS __attribute__((swift_attr("import_throws")))
#define FRT_IMMORTAL __attribute__((swift_attr("import_reference"))) \
  __attribute__((swift_attr("retain:immortal"))) \
  __attribute__((swift_attr("release:immortal")))

inline int methodCleanupCount = 0;
struct MethodCleanup {
  ~MethodCleanup() noexcept { ++methodCleanupCount; }
};
inline int getMethodCleanupCount() noexcept { return methodCleanupCount; }

struct Counter {
  int value = 10;
  int read(bool fail) const SWIFT_THROWS {
    if (fail) throw std::runtime_error("read failed");
    return value;
  }
  int add(int delta, bool fail) & SWIFT_THROWS {
    MethodCleanup cleanup;
    value += delta;
    if (fail) throw std::runtime_error("add failed");
    return value;
  }
  int take(bool fail) && SWIFT_THROWS {
    if (fail) throw std::runtime_error("take failed");
    return value;
  }
  void clear(bool fail) SWIFT_THROWS {
    value = 0;
    if (fail) throw 42;
  }
  int getCheckedValue() const SWIFT_THROWS { return value; }
  int getNoexceptValue() const noexcept SWIFT_THROWS { return value; }
  int unnamed(int) const SWIFT_THROWS { return value; }
};
struct DerivedCounter : Counter {};
struct TwiceDerivedCounter : DerivedCounter {};

struct QualifiedReceiver {
  int value = 80;
  int read(bool fail) const volatile & SWIFT_THROWS {
    if (fail) throw std::runtime_error("qualified receiver failed");
    return value;
  }
  int take(bool fail) const && SWIFT_THROWS {
    if (fail) throw std::runtime_error("qualified consume failed");
    return value;
  }
};

inline int receiverCopyCount = 0;
struct NontrivialReceiver {
  NontrivialReceiver() = default;
  NontrivialReceiver(const NontrivialReceiver &) noexcept { ++receiverCopyCount; }
  ~NontrivialReceiver() noexcept {}
  int checked(bool fail) const SWIFT_THROWS {
    if (fail) throw std::runtime_error("nontrivial receiver failed");
    return 60;
  }
  int take(bool fail) && SWIFT_THROWS {
    if (fail) throw std::runtime_error("nontrivial consume failed");
    return 61;
  }
};
inline int getReceiverCopyCount() noexcept { return receiverCopyCount; }

struct ThrowingReceiverCopy {
  ThrowingReceiverCopy(const ThrowingReceiverCopy &) noexcept(false) {}
  ThrowingReceiverCopy(ThrowingReceiverCopy &&) noexcept = default;
  int take() && SWIFT_THROWS { return 1; }
};
struct ThrowingReceiverMove {
  ThrowingReceiverMove(const ThrowingReceiverMove &) noexcept = default;
  ThrowingReceiverMove(ThrowingReceiverMove &&) noexcept(false) {}
  int take() && SWIFT_THROWS { return 1; }
};
struct ThrowingReceiverDestruction {
  ~ThrowingReceiverDestruction() noexcept(false) {}
  int take() && SWIFT_THROWS { return 1; }
};

inline int noncopyableDestructionCount = 0;
inline int getNoncopyableDestructionCount() noexcept {
  return noncopyableDestructionCount;
}
struct __attribute__((swift_attr("~Copyable"))) NoncopyableReceiver {
  int value = 70;
  NoncopyableReceiver() = default;
  NoncopyableReceiver(const NoncopyableReceiver &) = delete;
  NoncopyableReceiver(NoncopyableReceiver &&other) noexcept : value(other.value) {
    other.value = -1;
  }
  ~NoncopyableReceiver() noexcept {
    if (value >= 0) ++noncopyableDestructionCount;
  }
  int read(bool fail) const SWIFT_THROWS {
    if (fail) throw std::runtime_error("noncopyable receiver failed");
    return value;
  }
  int add(int delta) & SWIFT_THROWS { return value += delta; }
  int take(bool fail) && SWIFT_THROWS {
    if (fail) throw std::runtime_error("noncopyable consume failed");
    return value;
  }
};

struct ValueBase {
  virtual int checked(bool fail) const SWIFT_THROWS {
    if (fail) throw std::runtime_error("value base failed");
    return 20;
  }
};
struct ValueDerived : ValueBase {
  int checked(bool fail) const override SWIFT_THROWS {
    if (fail) throw std::runtime_error("value derived failed");
    return 30;
  }
};

struct FRT_IMMORTAL ReferenceBase {
  virtual int checked(bool fail) const SWIFT_THROWS {
    if (fail) throw std::runtime_error("reference base failed");
    return 40;
  }
  virtual ~ReferenceBase() = default;
};
struct ReferenceDerived : ReferenceBase {
  int checked(bool fail) const override SWIFT_THROWS {
    if (fail) throw std::runtime_error("reference derived failed");
    return 50;
  }
  static ReferenceDerived &create() {
    static ReferenceDerived value;
    return value;
  }
};
inline ReferenceBase &getReferenceBase() { return ReferenceDerived::create(); }

//--- check.swift
import AnnotatedThrowingMethods

func check(_ value: inout Counter) throws {
  _ = value.read(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = value.add(1, false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  let captured: (Bool) throws -> CInt = value.read
  _ = captured(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = try captured(false)
  _ = try value.getCheckedValue()
  _ = value.checkedValue // expected-error {{value of type 'Counter' has no member 'checkedValue'}}
}

func unsupportedReceivers(_ copy: consuming ThrowingReceiverCopy,
                          _ move: consuming ThrowingReceiverMove,
                          _ destruction: consuming ThrowingReceiverDestruction) {
  _ = copy.take() // expected-error {{'take()' is unavailable: SWIFT_THROWS on consuming methods requires nonthrowing receiver transfer and destruction}}
  _ = move.take() // expected-error {{'take()' is unavailable: SWIFT_THROWS on consuming methods requires nonthrowing receiver transfer and destruction}}
  _ = destruction.take() // expected-error {{'take()' is unavailable: SWIFT_THROWS on consuming methods requires nonthrowing receiver transfer and destruction}}
}

//--- use.swift
import AnnotatedThrowingMethods

public func read(_ value: Counter, fail: Bool) throws -> CInt {
  try value.read(fail)
}
public func add(_ value: inout Counter, fail: Bool) throws -> CInt {
  try value.add(1, fail)
}
public func take(_ value: consuming Counter, fail: Bool) throws -> CInt {
  try value.take(fail)
}

// SIL-LABEL: // Counter.read(_:)
// SIL: $@convention(method) {{.*}} @error any Error
// SIL: function_ref {{.*}}_withCxxExceptionCapture
// SIL: try_apply
// SIL-LABEL: // Counter.add(_:_:)
// SIL: $@convention(method) {{.*}} @inout Counter
// SIL: function_ref {{.*}}_withCxxExceptionCapture
// SIL: try_apply
// SIL-LABEL: // closure #1 in Counter.read(_:)
// SIL: function_ref {{.*}}__swift_cxx_exception_
// SIL-LABEL: // closure #1 in Counter.add(_:_:)
// SIL: function_ref {{.*}}__swift_cxx_exception_
// IR: invoke {{.*}}Counter{{.*}}read
// IR: landingpad
// IR: call {{.*}}__swift_cxx_report_current_exception
