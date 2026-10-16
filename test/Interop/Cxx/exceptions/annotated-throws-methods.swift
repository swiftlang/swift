// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -cxx-interop-getters-setters-as-properties -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/disabled.swift -I %t -cxx-interoperability-mode=default -cxx-interop-getters-setters-as-properties -verify -verify-ignore-unrelated
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
  int renamed(bool fail) const SWIFT_THROWS
      __attribute__((swift_name("checkedRead(_:)"))) {
    return read(fail);
  }
  int overloaded() const SWIFT_THROWS { return value; }
  int overloaded() SWIFT_THROWS { return ++value; }
  // Unsupported result type, so this stays unavailable.
  int *getPointer() const SWIFT_THROWS;
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

// Inherits checked() without overriding it.
struct ReferenceInheritsChecked : ReferenceBase {
  static ReferenceInheritsChecked &create() {
    static ReferenceInheritsChecked value;
    return value;
  }
};

struct FRT_IMMORTAL ReferenceCounter {
  int value = 90;
  int read(bool fail) const SWIFT_THROWS {
    if (fail) throw std::runtime_error("reference counter failed");
    return value;
  }
  int overloaded() const SWIFT_THROWS { return value; }
  int overloaded() SWIFT_THROWS { return ++value; }
  static ReferenceCounter &create() {
    static ReferenceCounter value;
    return value;
  }
};
struct ReferenceCounterDerived : ReferenceCounter {
  static ReferenceCounterDerived &create() {
    static ReferenceCounterDerived value;
    return value;
  }
};

// Only one of a base method and its override is annotated.
struct UnannotatedValueBase {
  virtual int mismatched(bool fail) const {
    if (fail) throw std::runtime_error("unannotated value base failed");
    return 1;
  }
};
struct AnnotatedValueOverride : UnannotatedValueBase {
  int mismatched(bool fail) const override SWIFT_THROWS {
    if (fail) throw std::runtime_error("annotated value override failed");
    return 2;
  }
};
struct AnnotatedValueBase {
  virtual int mismatched(bool fail) const SWIFT_THROWS {
    if (fail) throw std::runtime_error("annotated value base failed");
    return 3;
  }
};
struct UnannotatedValueOverride : AnnotatedValueBase {
  int mismatched(bool fail) const override {
    if (fail) throw std::runtime_error("unannotated value override failed");
    return 4;
  }
};

struct FRT_IMMORTAL UnannotatedReferenceBase {
  virtual int mismatched(bool fail) const {
    if (fail) throw std::runtime_error("unannotated reference base failed");
    return 5;
  }
  virtual ~UnannotatedReferenceBase() = default;
};
struct AnnotatedReferenceOverride : UnannotatedReferenceBase {
  int mismatched(bool fail) const override SWIFT_THROWS {
    if (fail) throw std::runtime_error("annotated reference override failed");
    return 6;
  }
  static AnnotatedReferenceOverride &create() {
    static AnnotatedReferenceOverride value;
    return value;
  }
};
inline UnannotatedReferenceBase &getUnannotatedReferenceBase() {
  return AnnotatedReferenceOverride::create();
}
struct FRT_IMMORTAL AnnotatedReferenceBase {
  virtual int mismatched(bool fail) const SWIFT_THROWS {
    if (fail) throw std::runtime_error("annotated reference base failed");
    return 7;
  }
  virtual ~AnnotatedReferenceBase() = default;
};
struct UnannotatedReferenceOverride : AnnotatedReferenceBase {
  int mismatched(bool fail) const override {
    if (fail) throw std::runtime_error("unannotated reference override failed");
    return 8;
  }
  static UnannotatedReferenceOverride &create() {
    static UnannotatedReferenceOverride value;
    return value;
  }
};
inline AnnotatedReferenceBase &getAnnotatedReferenceBase() {
  return UnannotatedReferenceOverride::create();
}

//--- check.swift
import AnnotatedThrowingMethods

func check(_ value: inout Counter) throws {
  _ = value.read(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = value.add(1, false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  let captured: (Bool) throws -> CInt = value.read
  _ = captured(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = try captured(false)
  _ = try value.getCheckedValue()
  _ = try value.checkedRead(false)
  _ = value.renamed(false) // expected-error {{value of type 'Counter' has no member 'renamed'}}
  _ = try value.overloaded()
  _ = try value.overloadedMutating()
  _ = value.checkedValue // expected-error {{value of type 'Counter' has no member 'checkedValue'}}
  _ = value.getPointer() // expected-error {{'getPointer()' is unavailable: SWIFT_THROWS currently requires arithmetic or enum parameters and an arithmetic or void result}}
  _ = value.pointer // expected-error {{value of type 'Counter' has no member 'pointer'}}
}

@available(SwiftStdlib 5.8, *)
func checkReferences() throws {
  let counter = ReferenceCounter.create()
  _ = counter.read(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = try counter.overloaded()
  _ = try counter.overloadedMutating()
  _ = ReferenceCounterDerived.create().read(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = ReferenceInheritsChecked.create().checked(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
}

// Each method throws according to its own annotation.
@available(SwiftStdlib 5.8, *)
func checkMismatchedOverrides() throws {
  _ = UnannotatedValueBase().mismatched(false)
  _ = AnnotatedValueOverride().mismatched(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = AnnotatedValueBase().mismatched(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = UnannotatedValueOverride().mismatched(false)
  _ = getUnannotatedReferenceBase().mismatched(false)
  _ = AnnotatedReferenceOverride.create().mismatched(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = getAnnotatedReferenceBase().mismatched(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = UnannotatedReferenceOverride.create().mismatched(false)
}

func unsupportedReceivers(_ copy: consuming ThrowingReceiverCopy,
                          _ move: consuming ThrowingReceiverMove,
                          _ destruction: consuming ThrowingReceiverDestruction) {
  _ = copy.take() // expected-error {{'take()' is unavailable: SWIFT_THROWS on consuming methods requires nonthrowing receiver transfer and destruction}}
  _ = move.take() // expected-error {{'take()' is unavailable: SWIFT_THROWS on consuming methods requires nonthrowing receiver transfer and destruction}}
  _ = destruction.take() // expected-error {{'take()' is unavailable: SWIFT_THROWS on consuming methods requires nonthrowing receiver transfer and destruction}}
}

//--- disabled.swift
import AnnotatedThrowingMethods

// Without CxxExceptionBridging the annotation has no effect, so annotated
// getters still become computed properties.
func checkDisabled(_ value: inout Counter) {
  _ = value.read(false)
  _ = value.checkedValue
  _ = value.pointer
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
@available(SwiftStdlib 5.8, *)
public func unannotatedBase(_ value: UnannotatedReferenceBase) -> CInt {
  value.mismatched(false)
}
@available(SwiftStdlib 5.8, *)
public func annotatedOverride(_ value: AnnotatedReferenceOverride) throws -> CInt {
  try value.mismatched(false)
}

// SIL-LABEL: // Counter.read(_:)
// SIL: $@convention(method) {{.*}} @error any Error
// SIL: function_ref {{.*}}_withCxxExceptionCapture
// SIL: try_apply
// SIL-LABEL: // Counter.add(_:_:)
// SIL: $@convention(method) {{.*}} @inout Counter
// SIL: function_ref {{.*}}_withCxxExceptionCapture
// SIL: try_apply
// A call through an unannotated base method uses the virtual method thunk
// directly, without an adapter. The annotated override gets its own facade.
// SIL-LABEL: sil {{.*}}@$s3use15unannotatedBase
// SIL-NOT: _withCxxExceptionCapture
// SIL: function_ref @{{.*}}UnannotatedReferenceBase{{.*}}mismatched{{.*}} : $@convention(cxx_method) (Bool, UnannotatedReferenceBase) -> Int32
// SIL-NOT: _withCxxExceptionCapture
// SIL: end sil function
// SIL-LABEL: sil {{.*}}@$s3use17annotatedOverride
// SIL: function_ref @$sSo26AnnotatedReferenceOverrideV10mismatchedys5Int32VSbKF{{.*}}U_ :
// SIL: function_ref {{.*}}_withCxxExceptionCapture
// SIL: end sil function
// SIL-LABEL: // closure #1 in Counter.read(_:)
// SIL: function_ref {{.*}}__swift_cxx_exception_
// SIL-LABEL: // closure #1 in Counter.add(_:_:)
// SIL: function_ref {{.*}}__swift_cxx_exception_
// IR: invoke {{.*}}Counter{{.*}}read
// IR: landingpad
// IR: call {{.*}}__swift_cxx_report_current_exception
