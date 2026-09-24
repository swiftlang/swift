// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -emit-ir %t/annotated.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -suppress-warnings -o /dev/null
// RUN: %target-build-swift %t/main.swift -I %t -o %t/test -cxx-interoperability-mode=default -Xfrontend -cxx-exception-mode=strict -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging -suppress-warnings
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test
// RUN: %target-build-swift %t/main.swift -I %t -o %t/test-opt -O -cxx-interoperability-mode=default -Xfrontend -cxx-exception-mode=strict -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging -suppress-warnings
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- module.modulemap
module SynthesizedInitializers {
  header "initializers.h"
  requires cplusplus
}

//--- initializers.h
struct Member {
  int value;
  explicit Member(int value) noexcept : value(value) {}
  Member(const Member &other) noexcept(false) : value(other.value) {
    if (value < 0) throw value;
  }
  Member(Member &&other) noexcept : value(other.value) {}
  ~Member() noexcept = default;
};
struct Aggregate { Member member; };
struct NoDefault {
  int value;
  explicit NoDefault(int value) noexcept : value(value) {}
  NoDefault(const NoDefault &other) noexcept(false) : value(other.value) {
    throw value;
  }
  NoDefault(NoDefault &&other) noexcept : value(other.value) {}
  ~NoDefault() noexcept = default;
};
// The enclosing union is safe to transfer, but its field initializer copies
// Member directly and must be rejected independently of the union's traits.
union MemberUnion {
  Member member;
  int value;
  MemberUnion() noexcept : value(0) {}
  MemberUnion(const MemberUnion &other) noexcept : value(other.value) {}
  ~MemberUnion() noexcept {}
};
union ArrayUnion {
  Member array[2];
  int value;
  ArrayUnion() noexcept : value(0) {}
  ArrayUnion(const ArrayUnion &other) noexcept : value(other.value) {}
  ~ArrayUnion() noexcept {}
};
static_assert(__is_nothrow_constructible(MemberUnion, const MemberUnion &),
              "the union's copy does not expose its member's throwing copy");
static_assert(__is_nothrow_constructible(ArrayUnion, const ArrayUnion &),
              "the union's copy does not expose its array's throwing copy");
struct CallbackHolder { int (*callback)(); };

struct SafeMember {
  int value;
  explicit SafeMember(int value) noexcept : value(value) {}
  SafeMember(const SafeMember &other) noexcept : value(other.value) {}
  ~SafeMember() noexcept = default;
};
struct SafeAggregate { SafeMember member; int value; };
struct SafeArray { int values[2]; };
struct SafeNoDefault { explicit SafeNoDefault(int value) noexcept : value(value) {} int value; };
union ScalarUnion { int value; double other; };
extern "C" {
struct CRecord { int (*callback)(); };
// A C linkage context does not make C++ member transfers nonthrowing.
struct CAggregate { Member member; };
union CMemberUnion {
  Member member;
  int value;
  CMemberUnion() noexcept : value(0) {}
  CMemberUnion(const CMemberUnion &other) noexcept : value(other.value) {}
  ~CMemberUnion() noexcept {}
};
}
static_assert(__is_nothrow_constructible(CMemberUnion, const CMemberUnion &),
              "the C linkage union still requires a separate member check");

//--- check.swift
import SynthesizedInitializers
func check(_ member: Member) {
  _ = Aggregate(member: member) // expected-error {{'init(member:)' is unavailable: synthesized C++ initializers require nonthrowing argument and result transfers in strict C++ exception mode}}
  _ = NoDefault() // expected-error {{'init()' is unavailable: synthesized C++ initializers require nonthrowing argument and result transfers in strict C++ exception mode}}
  _ = MemberUnion(member: member) // expected-error {{'init(member:)' is unavailable: synthesized C++ initializers require nonthrowing argument and result transfers in strict C++ exception mode}}
  _ = ArrayUnion(array: (member, member)) // expected-error {{'init(array:)' is unavailable: synthesized C++ initializers require nonthrowing argument and result transfers in strict C++ exception mode}}
  _ = CallbackHolder(callback: nil) // expected-error {{'init(callback:)' is unavailable: potentially throwing C++ callable types are not supported in strict C++ exception mode}}
  _ = CAggregate(member: member) // expected-error {{'init(member:)' is unavailable: synthesized C++ initializers require nonthrowing argument and result transfers in strict C++ exception mode}}
  _ = CMemberUnion(member: member) // expected-error {{'init(member:)' is unavailable: synthesized C++ initializers require nonthrowing argument and result transfers in strict C++ exception mode}}
}

//--- annotated.swift
import SynthesizedInitializers
// Preserve existing synthesized initializers in the default annotated mode.
func unchanged(_ member: Member) {
  _ = Aggregate(member: member)
  _ = NoDefault()
  _ = MemberUnion(member: member)
  _ = ArrayUnion(array: (member, member))
  _ = CallbackHolder(callback: nil)
  _ = CAggregate(member: member)
  _ = CMemberUnion(member: member)
}

//--- main.swift
import SynthesizedInitializers
let aggregate = SafeAggregate(member: SafeMember(17), value: 23)
precondition(aggregate.member.value == 17 && aggregate.value == 23)
let array = SafeArray(values: (3, 5))
precondition(array.values.0 == 3 && array.values.1 == 5)
precondition(SafeNoDefault().value == 0)
let scalarUnion = ScalarUnion(value: 42)
precondition(scalarUnion.value == 42)
// The safe field remains usable even when a different union field is rejected.
let memberUnion = MemberUnion(value: 19)
precondition(memberUnion.value == 19)
let cRecord = CRecord(callback: nil)
precondition(cRecord.callback == nil)
let cMemberUnion = CMemberUnion(value: 29)
precondition(cMemberUnion.value == 29)
