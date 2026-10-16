// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/const-class-members.swift -module-name ConstMembers -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/members.h
// RUN: %target-interop-build-clangxx -std=c++17 -c %s -I %t -o %t/legacy.o
// RUN: %target-interop-build-swift %S/const-class-members.swift -module-name ConstMembers -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xlinker %t/legacy.o -o %t/legacy
// RUN: %target-codesign %t/legacy
// RUN: %target-run %t/legacy | %FileCheck %s
// RUN: %target-swift-frontend %S/const-class-members.swift -module-name ConstMembers -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/members.h -enable-experimental-feature GenerateConstClassMembersInCXX
// RUN: %target-interop-build-clangxx -std=c++17 -DCONST_MEMBERS -c %s -I %t -o %t/const.o
// RUN: %target-interop-build-swift %S/const-class-members.swift -module-name ConstMembers -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xlinker %t/const.o -o %t/const
// RUN: %target-codesign %t/const
// RUN: %target-run %t/const | %FileCheck %s
// RUN: %target-swift-frontend %S/const-class-members.swift -module-name ConstMembers -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/members.h -enable-library-evolution -enable-experimental-feature GenerateConstClassMembersInCXX
// RUN: %target-interop-build-clangxx -std=c++17 -DCONST_MEMBERS -c %s -I %t -o %t/resilient.o
// RUN: %target-interop-build-swift %S/const-class-members.swift -module-name ConstMembers -enable-library-evolution -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xlinker %t/resilient.o -o %t/resilient
// RUN: %target-codesign %t/resilient
// RUN: %target-run %t/resilient | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateConstClassMembersInCXX

#include "members.h"
#include <cassert>
#include <type_traits>

#ifdef CONST_MEMBERS
#define MEMBER_CONST const
#else
#define MEMBER_CONST
#endif

using namespace ConstMembers;

// The flag must preserve existing member-function pointer types when disabled.
static_assert(
    std::is_same<decltype(&Counter::read),
                 swift::Int (Counter::*)() MEMBER_CONST noexcept>::value);
static_assert(
    std::is_same<decltype(&Counter::increment),
                 swift::Int (Counter::*)() MEMBER_CONST noexcept>::value);
static_assert(
    std::is_same<decltype(&Counter::consumeAndRead),
                 swift::Int (Counter::*)() MEMBER_CONST noexcept>::value);
static_assert(
    std::is_same<decltype(&Counter::setValue),
                 void (Counter::*)(swift::Int) MEMBER_CONST noexcept>::value);
static_assert(std::is_same<decltype(&ValueCounter::increment),
                           void (ValueCounter::*)() noexcept>::value);

extern "C" size_t swift_retainCount(void *);

int main() {
  {
    MEMBER_CONST auto derived = DerivedCounter::init(10);
    MEMBER_CONST Counter &counter = derived;
    void *pointer =
        swift::_impl::_impl_RefCountedClass::getOpaquePointer(counter);
    assert(swift_retainCount(pointer) == 1);

    // Dispatch through a borrowed base wrapper without copying the handle.
    assert(counter.read() == 20);
    assert(counter.increment() == 2);
    assert(counter.increment() == 4);
    assert(counter.finalRead() == 10);
    counter.setValue(15);
    assert(counter.getValue() == 15);
    counter.setComputed(42);
    assert(counter.getComputed() == 42);
    assert(counter.getValue() == 21);
    assert(counter[2] == 23);

    // Consuming thunks hand Swift a retained copy and preserve the C++ handle.
    assert(counter.consumeAndRead() == 42);
    assert(counter.consumeAndRead() == 42);
    assert(counter.read() == 42);
    assert(swift::_impl::_impl_RefCountedClass::getOpaquePointer(counter) ==
           pointer);
    assert(swift_retainCount(pointer) == 1);
    assert(Counter::answer() == 42);
  }
  // CHECK: destroy Counter
  // CHECK-NOT: destroy Counter

  auto value = ValueCounter::init(3);
  value.increment();
  const ValueCounter &borrowedValue = value;
  assert(borrowedValue.read() == 4);
  return 0;
}
