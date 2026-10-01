// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/disabled.swift -I %t -cxx-interoperability-mode=default -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/consteval.swift -I %t -cxx-interoperability-mode=default -Xcc -std=c++20 -enable-experimental-feature CxxExceptionBridging -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -emit-silgen %t/use.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -strict-memory-safety -warnings-as-errors | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -emit-ir %t/use.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging | %FileCheck %s --check-prefix=IR

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- module.modulemap
module AnnotatedThrowingConstructors {
  header "constructors.h"
  requires cplusplus
}

//--- constructors.h
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <new>
#include <stdexcept>

#define SWIFT_THROWS __attribute__((swift_attr("import_throws")))

inline int liveMembers = 0;
inline int liveResults = 0;
inline int memberDestructions = 0;
inline int resultDestructions = 0;

struct Member {
  Member() noexcept { ++liveMembers; }
  Member(const Member &) noexcept { ++liveMembers; }
  Member(Member &&) noexcept { ++liveMembers; }
  ~Member() noexcept {
    --liveMembers;
    ++memberDestructions;
  }
};

struct Checked {
  Member member;
  int value;
  explicit Checked(int value) SWIFT_THROWS : value(value) {
    if (value < 0)
      throw std::runtime_error("construction failed");
    ++liveResults;
  }
  explicit Checked(double value) SWIFT_THROWS
      : value(static_cast<int>(value) + 100) {
    if (value < 0)
      throw std::runtime_error("double construction failed");
    ++liveResults;
  }
  Checked(const Checked &other) noexcept
      : member(other.member), value(other.value) {
    ++liveResults;
  }
  Checked(Checked &&other) noexcept
      : member(static_cast<Member &&>(other.member)), value(other.value) {
    ++liveResults;
  }
  ~Checked() noexcept {
    --liveResults;
    ++resultDestructions;
  }
  // The compiler must select global placement new, not a class-specific hook.
  static void *operator new(std::size_t, void *) { std::abort(); }
};

struct __attribute__((swift_attr("~Copyable"))) MoveOnly {
  Member member;
  int value;
  explicit MoveOnly(int value) SWIFT_THROWS : value(value) {
    if (value < 0)
      throw 42;
    ++liveResults;
  }
  MoveOnly(const MoveOnly &) = delete;
  MoveOnly(MoveOnly &&other) noexcept
      : member(static_cast<Member &&>(other.member)), value(other.value) {
    ++liveResults;
  }
  ~MoveOnly() noexcept {
    --liveResults;
    ++resultDestructions;
  }
};

struct Inherited : Checked {
  using Checked::Checked;
};

struct ExplicitDerived : Checked {
  explicit ExplicitDerived(int value) SWIFT_THROWS : Checked(value) {}
};

class PrivateValue {
  Member member;
  int value;

public:
  explicit PrivateValue(int value) SWIFT_THROWS : value(value) {
    if (value < 0)
      throw std::runtime_error("private construction failed");
    ++liveResults;
  }
  PrivateValue(const PrivateValue &other) noexcept
      : member(other.member), value(other.value) {
    ++liveResults;
  }
  PrivateValue(PrivateValue &&other) noexcept
      : member(static_cast<Member &&>(other.member)), value(other.value) {
    ++liveResults;
  }
  ~PrivateValue() noexcept {
    --liveResults;
    ++resultDestructions;
  }
};

struct EmptyNontrivial {
  explicit EmptyNontrivial(int value) SWIFT_THROWS {
    if (value < 0)
      throw std::runtime_error("empty nontrivial construction failed");
    ++liveResults;
  }
  EmptyNontrivial(const EmptyNontrivial &) noexcept { ++liveResults; }
  EmptyNontrivial(EmptyNontrivial &&) noexcept { ++liveResults; }
  ~EmptyNontrivial() noexcept {
    --liveResults;
    ++resultDestructions;
  }
};

struct __attribute__((swift_attr("~Copyable"))) EmptyMoveOnly {
  explicit EmptyMoveOnly(int value) SWIFT_THROWS {
    if (value < 0)
      throw std::runtime_error("empty move-only construction failed");
    ++liveResults;
  }
  EmptyMoveOnly(const EmptyMoveOnly &) = delete;
  EmptyMoveOnly(EmptyMoveOnly &&) noexcept { ++liveResults; }
  ~EmptyMoveOnly() noexcept {
    --liveResults;
    ++resultDestructions;
  }
};

struct CopyFallback {
  int value;
  explicit CopyFallback(int value) SWIFT_THROWS : value(value) {}
  CopyFallback(const CopyFallback &other) noexcept : value(other.value) {}
};

struct alignas(16) Aligned {
  int value;
  explicit Aligned(int value) SWIFT_THROWS : value(value) {
    if (reinterpret_cast<std::uintptr_t>(this) % alignof(Aligned))
      std::abort();
  }
};

struct Empty {
  Empty() SWIFT_THROWS { throw std::runtime_error("empty failed"); }
};

struct Unnamed {
  explicit Unnamed(int) SWIFT_THROWS { throw 42; }
};

struct ZeroInitialized {
  int value;
  ZeroInitialized() SWIFT_THROWS = default;
};

struct FailingMember {
  explicit FailingMember(bool fail) {
    if (fail)
      throw std::runtime_error("member construction failed");
  }
};
struct Partial {
  Member first;
  FailingMember second;
  explicit Partial(bool fail) SWIFT_THROWS : second(fail) {}
};

inline void failNoexceptConstruction() { throw 42; }
struct NoexceptConstruction {
  explicit NoexceptConstruction(bool fail) noexcept SWIFT_THROWS {
    if (fail)
      failNoexceptConstruction();
  }
};

struct ThrowingCopy {
  explicit ThrowingCopy(int) SWIFT_THROWS {}
  ThrowingCopy(const ThrowingCopy &) noexcept(false) {}
  ThrowingCopy(ThrowingCopy &&) noexcept = default;
};
struct ThrowingMove {
  explicit ThrowingMove(int) SWIFT_THROWS {}
  ThrowingMove(const ThrowingMove &) noexcept = default;
  ThrowingMove(ThrowingMove &&) noexcept(false) {}
};
struct ThrowingDestructor {
  explicit ThrowingDestructor(int) SWIFT_THROWS {}
  ~ThrowingDestructor() noexcept(false) {}
};
struct DeletedMove {
  explicit DeletedMove(int) SWIFT_THROWS {}
  DeletedMove(const DeletedMove &) noexcept = default;
  DeletedMove(DeletedMove &&) = delete;
};
struct WithDefault {
  explicit WithDefault(int value = 1) SWIFT_THROWS {}
};
struct __attribute__((swift_attr("~Escapable"))) NonEscapable {
  explicit NonEscapable(int) SWIFT_THROWS {}
};

template <class T>
struct Box {
  T value;
  explicit Box(int value) SWIFT_THROWS : value(value) {
    if (value < 0)
      throw std::runtime_error("box construction failed");
  }
};
using IntBox = Box<int>;

struct __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:immortal")))
    __attribute__((swift_attr("release:immortal"))) Reference {
  explicit Reference(int value) SWIFT_THROWS {}
};

#if defined(__cpp_consteval)
struct Immediate {
  consteval explicit Immediate(int value) SWIFT_THROWS {}
};
#endif

inline int getLiveMembers() noexcept { return liveMembers; }
inline int getLiveResults() noexcept { return liveResults; }
inline int getMemberDestructions() noexcept { return memberDestructions; }
inline int getResultDestructions() noexcept { return resultDestructions; }

//--- check.swift
import AnnotatedThrowingConstructors

func check() throws {
  _ = Checked(CInt(1)) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = NoexceptConstruction(false) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  let _: (CInt) throws -> Checked = Checked.init
  let _: () throws -> Empty = Empty.init
  _ = ZeroInitialized() // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = try CopyFallback(1)
  _ = try Aligned(1)
  _ = try MoveOnly(1)
  _ = Inherited(CInt(1)) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
  _ = ThrowingCopy(1) // expected-error {{'init(_:)' is unavailable: SWIFT_THROWS constructors require nonthrowing destruction and move construction, and nonthrowing copy construction for copyable types}}
  _ = ThrowingMove(1) // expected-error {{'init(_:)' is unavailable: SWIFT_THROWS constructors require nonthrowing destruction and move construction, and nonthrowing copy construction for copyable types}}
  _ = ThrowingDestructor(1) // expected-error {{'init(_:)' is unavailable: SWIFT_THROWS constructors require nonthrowing destruction and move construction, and nonthrowing copy construction for copyable types}}
  _ = DeletedMove(1) // expected-error {{'init(_:)' is unavailable: SWIFT_THROWS constructors require nonthrowing destruction and move construction, and nonthrowing copy construction for copyable types}}
  _ = WithDefault(1) // expected-error {{'init(_:)' is unavailable: SWIFT_THROWS on functions with default arguments is not yet supported}}
  _ = NonEscapable(1) // expected-error {{'init(_:)' is unavailable: SWIFT_THROWS constructors currently require an escapable C++ value type}}
  _ = IntBox(1) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{did you mean to use 'try'?}} expected-note {{did you mean to handle error as optional value?}} expected-note {{did you mean to disable error propagation?}}
}

@available(SwiftStdlib 5.8, *)
func checkReference() {
  _ = Reference(1) // expected-error {{'init(_:)' is unavailable: SWIFT_THROWS is not supported on this kind of declaration}}
}

//--- disabled.swift
import AnnotatedThrowingConstructors

// Without CxxExceptionBridging the annotation has no effect.
func disabled() {
  _ = Checked(CInt(1))
  _ = Inherited(CInt(1))
  _ = ThrowingCopy(1)
}

@available(SwiftStdlib 5.8, *)
func disabledReference() {
  _ = Reference(1)
}

//--- use.swift
import AnnotatedThrowingConstructors
public func construct(_ value: CInt) throws -> Checked {
  try Checked(value)
}
public func constructorReference() -> (CInt) throws -> Checked {
  Checked.init
}
public func constructMoveOnly(_ value: CInt) throws -> MoveOnly {
  try MoveOnly(value)
}
public func constructEmptyNontrivial(_ value: CInt) throws -> EmptyNontrivial {
  try EmptyNontrivial(value)
}

//--- consteval.swift
import AnnotatedThrowingConstructors
func immediate(_ value: CInt) {
  _ = Immediate(value) // expected-error {{'init(_:)' is unavailable: SWIFT_THROWS is not supported on consteval functions}}
}

// The facade initializes all of self at once from the adapter's result, so a
// failed construction never destroys uninitialized storage.
// SIL-LABEL: sil shared [transparent] {{.*}}@$sSo7CheckedVyABs5Int32VKcfC : $@convention(method) (Int32, @thin Checked.Type) -> (@out Checked, @error any Error) {
// SIL: mark_uninitialized [delegatingself]
// SIL: // function_ref _withCxxExceptionResult
// SIL: try_apply {{.*}}<Checked>
// SIL: } // end sil function '$sSo7CheckedVyABs5Int32VKcfC'
// SIL-LABEL: sil shared [transparent] {{.*}}@$sSo15EmptyNontrivialVyABs5Int32VKcfC :
// SIL: mark_uninitialized [delegatingself]
// SIL: } // end sil function '$sSo15EmptyNontrivialVyABs5Int32VKcfC'
// SIL-LABEL: sil shared {{.*}}@$sSo7CheckedVyABs5Int32VKcfcySvSg_{{.*}}fU_ :
// SIL: function_ref @$sSo{{[0-9]+}}__swift_cxx_exception_5F5A4E37436865636B656443314569{{.*}} : $@convention(c) (Int32, Optional<UnsafeMutableRawPointer>, Optional<UnsafeMutableRawPointer>, Optional<@convention(c) {{.*}}>) -> ()
// SIL: } // end sil function

// The adapter constructs the value in the storage it receives.
// IR-LABEL: define internal void @{{.*}}__swift_cxx_exception_5F5A4E37436865636B656443314569{{.*}}(i32 {{.*}}%arg0, ptr {{.*}}%arg1, ptr {{.*}}%arg2, ptr {{.*}}%arg3)
// IR: [[OUTPUT:%.*]] = load ptr, ptr %arg1.addr
// IR: invoke {{.*}}@_ZN7CheckedC1Ei(ptr {{.*}}[[OUTPUT]], i32
// IR: catch ptr null
// IR: call void @{{.*}}__swift_cxx_report_current_exception
